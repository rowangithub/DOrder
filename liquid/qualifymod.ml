(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

open Asttypes
open Typedtree
open Parsetree
open Btype
open Types
open Constraint
open Longident
open Location
open Format

open Backwalker
open Cbslearner

module P = Predicate
module C = Common
module Cf = Clflags
module B = Builtins
module Le = Lightenv
module F = Frame
module W = Wellformed

type error =
  | NotSubtype of F.t * F.t
  | IllFormed of F.t
  | AssertMayFail

exception Error of Location.t * error
exception Errors of (Location.t * error) list

exception NoRun

let expression_to_pexpr e =
  match e.exp_desc with
    | Texp_constant (Const_int n) -> P.PInt n
    | Texp_ident (id, _) -> P.Var id
    | _ -> P.Var (Path.mk_ident "dummy")

let under_lambda = ref 0

let effect_test_var = Path.mk_ident "effect_id"
let effect_test_expr = Predicate.Var effect_test_var

let forall_uvar = Datatype.forall_uvar
let forall_uexpr = Datatype.forall_uexpr
let forall_vvar = Datatype.forall_vvar
let forall_vexpr = Datatype.forall_vexpr
let foralls = Datatype.foralls
(* save the construction of a data type for generating*)
(* its link and reach predicates *)
let udt_table = Hashtbl.create 3


module FrameExpLog = Map.Make(struct type t = Typedtree.expression
                     					let compare = compare end)
let fexplog = ref FrameExpLog.empty
let log_exp_frame exp fr = 
	fexplog := FrameExpLog.add exp fr !fexplog

module FrameLog = Map.Make(struct type t = Location.t
                                  let compare = compare end)
let flog = ref FrameLog.empty

let log_frame loc fr =
  if loc <> Location.none then 
    flog := FrameLog.add loc fr !flog

let framemap_apply_solution s fmap = FrameLog.map (F.apply_solution s) fmap

let dump_frame pp loc fr =
  if loc.loc_start <> Lexing.dummy_pos && loc.loc_end <> Lexing.dummy_pos then
    Stypes.print_position pp loc.loc_start;
    fprintf pp " ";
    Stypes.print_position pp loc.loc_end;
    fprintf pp "@.type(@.  ";
    F.pprint pp fr;
    fprintf pp "@.)@."

let dump_frames sourcefile fmap =
  if !Cf.dump_frames then
    let filename = Misc.chop_extension_if_any sourcefile ^ ".annot" in
    let pp = formatter_of_out_channel (open_out filename) in
      FrameLog.iter (dump_frame pp) fmap

let label_constraint exp fc =
  let org = match exp.exp_desc with Texp_assert _ -> Assert exp.exp_loc | _ -> Loc exp.exp_loc in
    {lc_cstr = fc; lc_tenv = exp.exp_env; lc_orig = org; lc_id = fresh_fc_id()}

let is_poly_instantiation = function
  | (Texp_construct _, _)
  | (Texp_ident _, _)
  | (Texp_assertfalse, _) -> true
  | _ -> false

let expr_fresh desc_and_ty = if is_poly_instantiation desc_and_ty then Frame.fresh_unconstrained else Frame.fresh

let rec get_funbody exp = match exp.exp_desc with
	| Texp_function ([(pat, e')], _) -> get_funbody e'
	| _ -> exp 

(** bind function body and function frame for symbolic execution use *)
let se_env_bind_fun env se_env recflag pat exp fr =
	match (pat.pat_desc, exp) with 
		| (Tpat_var id, {exp_desc = Texp_function (_, Total)}) -> 
			let bodyexp = get_funbody exp in (
			Hashtbl.replace se_env.funbindings (Path.Pident id) (recflag, bodyexp);
			Hashtbl.replace se_env.funframebindings (Path.Pident id) fr;
			Hashtbl.replace se_env.fundefenvs (Path.Pident id) (Lightenv.bindings env)
			)
		| _ -> ()

let rec collect_fpath e1 = match e1.exp_desc with
	| Texp_apply (e1', exps) -> collect_fpath e1'
	| Texp_ident (id, _) -> id
	| _ -> 
		(fprintf err_formatter 
		"@[Warning: Dont know how to deal with non-simple lambda@]@.@.";
		assert false)

(** If this is a full function application, bind the calling context for symbolic execution use  *)
let se_env_bind_app se_env e env appframe = match appframe with
	| Frame.Farrow (Some _,_,_,_) -> (** partial *) ()
	| Frame.Farrow (None, _,_,_) -> 
		(* Fixme? If the called function is named, then full application it is
		   otherwise it is considered as partial *)
		let fpath = collect_fpath e in
		if (Hashtbl.mem se_env.funbindings fpath) then (** full *) (
			if (List.exists (fun p -> Path.same p fpath) se_env.builtin_funs) then ()
			else (** Only care user-defined function *)
			Hashtbl.replace se_env.funcallenvs e.exp_loc (Lightenv.bindings env)	
		)
		else (** partial *) ()
	| _ -> (** full *)
		let fpath = collect_fpath e in
		if (List.exists (fun p -> Path.same p fpath) se_env.builtin_funs) then ()
		else (** Only care user-defined function and higher order function application *)
			Hashtbl.replace se_env.funcallenvs e.exp_loc (Lightenv.bindings env)	

(* quickly check if a function is a measure *)						
let quickcheck_measures	se_env fpath = 
	(String.compare "List.length" (Path.name fpath) = 0) ||
	(let datatypes = Hashtbl.fold (fun p _ res -> p::res) se_env.measures [] in
	try (let fr = Hashtbl.find se_env.funframebindings fpath in
	match fr with
	| Frame.Farrow (_, Frame.Fconstr (p1,_,_,_,_), Frame.Fconstr (p2,_,_,_,_), _) 
		when (List.exists (fun dty -> Path.same p1 dty) datatypes && Path.same p2 Predef.path_int)-> true
	| _ -> false) with _ -> false		)																											
			
(** Add higher order function (array) encoding to support local reasoning *)
let encode_higher_order_function se_env env recflag bindings = 
	(*let rec count_ho_args t = match repr t with
		| {desc = Tarrow (_,_,t,_)} -> 1 + count_ho_args t
		| _ -> 0 in *)
	let encode p t fpath exps env = 
		(*let n = count_ho_args t + 1 in
		let hoargs = Array.to_list (Array.init n (fun i -> P.Var (F.get_ho_param i))) in
		let fun_encoding = P.FunApp ("Ret"^(string_of_int (1+List.length exps+n)), 
			(P.Var fpath) :: ((List.map expression_to_pexpr exps) @ hoargs)) in
		Le.add (Path.mk_ident "hoencoding")
		(B.mk_int [(Path.mk_ident "", Path.mk_ident "", Pattern.desugar_bind p.pat_desc fun_encoding)])*)
		env in 
	List.fold_left (fun env (p, e) -> match e.exp_desc with
		| Texp_apply (e1', exps) ->
			let fpath = collect_fpath e in
			let exps = List.map (fun exp -> match exp with
				| (Some e2, _) -> e2
				| _ -> assert false) exps in
			if (quickcheck_measures se_env (fpath)) then (* encoding measures *)
					let fun_encoding = P.FunApp (Path.name fpath, 
						(List.map expression_to_pexpr exps)) in
					let fun_encoding = Pattern.desugar_bind p.pat_desc fun_encoding in
					Le.add (Path.mk_ident "hoencoding")
        	(B.mk_int [(Path.mk_ident "", Path.mk_ident "", fun_encoding)])
        	env
			else if (Hashtbl.mem se_env.funbindings fpath) then (
				match repr e.exp_type with 
					| {desc = Tarrow (_,_,t,_)} -> encode p t fpath exps env
					| _ -> env)
			else if (List.exists (fun p -> Path.same p fpath) se_env.builtin_funs) then 
				(** Also, encode array get *)
				if ((String.compare (Path.name fpath) "Array.get") = 0 && List.length exps = 2) then
					(** For_all variables is encoded by ho variable *)
					let params = List.map (fun exp -> expression_to_pexpr exp) exps in
					(*let param_encodings = List.fold_left (fun res exp -> match exp.exp_desc with
						| Texp_ident (id, _) when (String.compare (Path.name id) "ith" = 0) -> 
							res @ [P.Atom (P.Var id, P.Eq, P.Var (F.get_ho_param 0))]
						| _ -> res) [] exps in*)
					let arrayget_encoding = P.FunApp ("Ret2", params) in
					let arrayget_encoding = Pattern.desugar_bind p.pat_desc arrayget_encoding in
					let arrayget_encoding = 
						(*if (List.length param_encodings > 0) then 
							Predicate.big_and (param_encodings@[arrayget_encoding])
						else*) arrayget_encoding in
					Le.add (Path.mk_ident "arraygetencoding")
					(B.mk_int [(Path.mk_ident "", Path.mk_ident "", arrayget_encoding)])
					env
				else env
			else (match repr e.exp_type with
				| {desc = Tarrow (_,_,t,_)} -> (
					(* Fixme: do not know how to deal partial application idealy! *)
					encode p t fpath exps env
					)
				| _ -> 
					let fun_encoding = P.FunApp ("Ret"^(string_of_int (1+List.length exps)), 
						(P.Var fpath) :: (List.map expression_to_pexpr exps)) in
					(** optionally encode for_all variables *)
					let (_, param_encodings) = List.fold_left (fun (i,res) exp -> match exp.exp_desc with
						| Texp_ident (id, _) -> 
							if (String.compare (Path.name id) "ith" = 0) then
							(i+1, res @ [P.Atom (
								P.Var id, P.Eq, 
								P.FunApp("Arg2", [(P.Var fpath); (*P.Var (F.get_ho_param i)*)P.PInt i]))])
							else (i+1, res)
						| _ -> (i+1, res) (* Fixme? *)
						) (0,[]) exps in
					let fun_encoding = Pattern.desugar_bind p.pat_desc fun_encoding in
					let fun_encoding = 
						if (List.length param_encodings > 0) then 
							Predicate.big_and (param_encodings@[fun_encoding])
						else fun_encoding in
					Le.add (Path.mk_ident "hoencoding")
        	(B.mk_int [(Path.mk_ident "", Path.mk_ident "", fun_encoding)])
        	env
			)
		| _ -> env
	) env bindings

(** Invariant: effect is only useful when constrain a function expression *)	
let rec constrain inherit_effect se_env e env guard =
  let desc_ty = (e.exp_desc, repr e.exp_type) in
  let environment = (env, guard, expr_fresh desc_ty e.exp_env e.exp_type) in
  let (f, cstrs, rec_cstrs, effobjs) =
    match desc_ty with
      | (Texp_constant const_typ, {desc = Tconstr(path, [], _)}) -> constrain_constant path const_typ
      | (Texp_construct (cstrdesc, args), {desc = Tconstr(_, _, _)}) -> constrain_constructed se_env environment cstrdesc args e
      | (Texp_record (labeled_exprs, None), {desc = (Tconstr _)}) -> constrain_record se_env environment labeled_exprs
      | (Texp_field (expr, label_desc), _) -> constrain_field se_env environment expr label_desc
      | (Texp_ifthenelse (e1, e2, Some e3), _) -> constrain_if se_env environment e1 e2 e3
      | (Texp_match (e, pexps, partial), _) -> constrain_match se_env environment e pexps partial
      | (Texp_function ([(pat, e')], _), t) -> constrain_function inherit_effect se_env environment t pat e'
      | (Texp_ident (id, _), {desc = Tconstr (p, [], _)} ) -> constrain_base_identifier environment id e
      | (Texp_ident (id, _), _) -> constrain_identifier environment id e.exp_env
      | (Texp_apply (e1, exps), _) -> 
				let (app_f, app_cstrs, app_rec_cstrs, effobjs) = constrain_application se_env environment e1 exps in
				let _ = se_env_bind_app se_env e env app_f in
				(app_f, app_cstrs, app_rec_cstrs, effobjs)
      | (Texp_let (recflag, bindings, body_exp), t) -> constrain_let se_env environment recflag bindings body_exp
      | (Texp_array es, _) -> constrain_array se_env environment es
      | (Texp_sequence (e1, e2), _) -> constrain_sequence se_env environment e1 e2
      | (Texp_tuple es, _) -> constrain_tuple se_env environment es
      | (Texp_assertfalse, _) -> constrain_assertfalse environment
      | (Texp_assert e, _) -> constrain_assert se_env environment e
      | (_, t) ->
        fprintf err_formatter "@[Warning: Don't know how to constrain expression,
        structure:@ %a@ location:@ %a@]@.@." Printtyp.raw_type_expr t Location.print e.exp_loc; flush stderr;
        assert false in
  log_frame e.exp_loc f; log_exp_frame e f; (f, (List.map (label_constraint e) cstrs) @ rec_cstrs, effobjs)

and constrain_constant path = function
  | Const_int n ->
      (B.mk_int [B.equality_qualifier (P.PInt n)], [], [], [])
  | Const_float _ -> (B.uFloat, [], [], [])
  | Const_char _ -> (B.uChar, [], [], [])
  | Const_string _ -> (B.uString, [], [], [])
  | _ -> assert false

and constrain_constructed se_env (env, guard, f) cstrdesc args e =
  match f with
  | F.Fconstr (path, tyargframes, cstrs, _, effect) ->
			let _ = assert (List.length effect = 0) in
      let tag = cstrdesc.cstr_tag in
      let cstrref = match tag with
        | Cstr_constant n -> B.tag_refinement n | Cstr_block n -> B.tag_refinement n
        | Cstr_exception _ -> assert false
      in
      let f = F.Fconstr (path, tyargframes, cstrs, cstrref, effect) in
      let cstrargs = F.fresh_constructor e.exp_env cstrdesc f in
      let (argframes, argcstrs, effobjss) = constrain_subexprs se_env env guard args in
			let cstrargs = List.map2 (fun formal effobj -> 
				Frame.push_effect e.exp_env env formal effobj) cstrargs effobjss in
			let f = (** If constructed from [], list element type is \false/; encode list length *)
				if (Path.same path Predef.path_list && List.length args = 0) then
					let tyargframes = List.map (fun tyframe -> 
						F.set_refinement tyframe F.false_refinement) tyargframes in
					let measure_refinement = 	
						Predicate.Atom (Predicate.FunApp ("List.length", [Constraint.qual_test_expr]), Predicate.Eq, Predicate.PInt 0) in
					let measure_refinement = 
						if !(Clflags.reachability) then 
							let link_refinement1 = 
								Predicate.logic_equals (Predicate.Reach (Constraint.qual_test_expr,forall_uexpr))
								(Predicate.Not Predicate.True) in
							let link_refinement2 = 
								(** see whether the list is a container of containers *)
								let (b, p) = Datatype.extract_value_type_from_container_fr f in
								if (b) then 
									Predicate.logic_equals (Predicate.Link (Constraint.qual_test_expr,"cons",1,forall_uexpr,forall_vexpr))
									(Predicate.Not Predicate.True)
								else 
									let sublinks = Datatype.get_all_links udt_table p in
									Predicate.big_and (List.map (fun (n, i) ->
										let n = String.lowercase_ascii n in
										Predicate.logic_equals (Predicate.Link (Constraint.qual_test_expr,n,i,forall_uexpr,forall_vexpr))
										(Predicate.Not Predicate.True)
										) sublinks) in
							let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
							let link_refinement2 = Predicate.Forall (foralls, link_refinement2) in
							Predicate.big_and [link_refinement1; link_refinement2; measure_refinement]
						else measure_refinement in	
					F.update_refinement (F.Fconstr (path, tyargframes, cstrs, cstrref, effect))
					(Path.mk_ident "", Constraint.qual_test_var, measure_refinement)
				else if (Path.same path Predef.path_list && List.length args > 0) then
					let tl = List.nth args 1 in
					let tl = expression_to_pexpr tl in
					(** A link relation for list *)
					let measure_refinement = Predicate.And ( 
						Predicate.Atom (Predicate.FunApp ("List.length", [Constraint.qual_test_expr]), Predicate.Eq, 
							Predicate.Binop (Predicate.PInt 1, Predicate.Plus, Predicate.FunApp ("List.length", [tl]))),
						Predicate.Atom (Predicate.FunApp ("List.length", [tl]), Predicate.Ge, Predicate.PInt 0)	
						) in
					let measure_refinement = 
						if !(Clflags.reachability) then 
							let hd = expression_to_pexpr (List.nth args 0) in
							(** see whether the list is a container of containers *)
							let (b, p) = Datatype.extract_value_type_from_container_fr f in
							let sublinks = if b then [] else Datatype.get_all_links udt_table p in
							let link_refinement1 = 
								if (b) then 
									Predicate.logic_equals (Predicate.Reach (Constraint.qual_test_expr,forall_uexpr))
										(Predicate.Or (Predicate.Atom (hd,Predicate.Eq,forall_uexpr), Predicate.Reach (tl,forall_uexpr))
									) 
								else 
										Predicate.logic_equals (Predicate.Reach (Constraint.qual_test_expr,forall_uexpr))
										(Predicate.Or (Predicate.Reach (hd,forall_uexpr), Predicate.Reach (tl,forall_uexpr))
										) in
							let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
							let link_refinement2 = 
								if (b) then
									Predicate.logic_equals (Predicate.Link (Constraint.qual_test_expr,"cons",1,forall_uexpr,forall_vexpr))
										(Predicate.Or ( 
											Predicate.And (Predicate.Atom (hd,Predicate.Eq,forall_uexpr), Predicate.Reach (tl, forall_vexpr)),
											Predicate.Link (tl,"cons",1,forall_uexpr,forall_vexpr))
									)
								else
									Predicate.big_and (List.map (fun (n, i) -> 
										let n = String.lowercase_ascii n in
										Predicate.logic_equals (Predicate.Link (Constraint.qual_test_expr,n,i,forall_uexpr,forall_vexpr))
										(Predicate.Or ( 
											Predicate.Link (hd,n,i,forall_uexpr,forall_vexpr),
											Predicate.Link (tl,n,i,forall_uexpr,forall_vexpr))
										)) sublinks) in	
							let link_refinement2 = Predicate.Forall (foralls, link_refinement2) in
							Predicate.big_and [link_refinement1; link_refinement2; measure_refinement]
						else measure_refinement in
					F.update_refinement f (Path.mk_ident "", Constraint.qual_test_var, measure_refinement)  
				else if (Hashtbl.mem se_env.measures path) then (* User defined data type *)
					let cstrname = cstrdesc.cstr_name in
					F.update_refinement f (Path.mk_ident "", Constraint.qual_test_var, 
						Predicate.Atom (Constraint.qual_test_expr, Predicate.Eq, 
							Predicate.FunApp (cstrname, List.map (expression_to_pexpr) args)))
				else f in
	    (f,
	     WFFrame(env, f) :: (List.map2 (fun arg formal -> SubFrame(env, guard, arg, formal)) argframes cstrargs),
	     argcstrs, Common.remove_duplicates (List.flatten effobjss))
  | _ -> assert false

and constrain_record se_env (env, guard, f) labeled_exprs =
  let compare_labels ({lbl_pos = n}, _) ({lbl_pos = m}, _) = compare n m in
  let (_, sorted_exprs) = List.split (List.sort compare_labels labeled_exprs) in
  let (subframes, subexp_cs, effobjss) = constrain_subexprs se_env env guard sorted_exprs in
  let subframe_field cs_rest fsub (fsup, _, _) = SubFrame (env, guard, fsub, fsup) :: cs_rest in
  match f with
    | F.Frecord (p, recframes, _) ->
			let recframes = List.map2 (fun (recframe,n,m) effobjs -> 
				(Frame.push_effect (snd (List.hd labeled_exprs)).exp_env env recframe effobjs, n, m)) recframes effobjss in
      let field_qualifier (_, name, _) fexpr = B.field_eq_qualifier name (expression_to_pexpr fexpr) in
        (F.Frecord (p, recframes, ([], F.Qconst (List.map2 field_qualifier recframes sorted_exprs))),
         WFFrame (env, f) :: List.fold_left2 subframe_field [] subframes recframes,
         subexp_cs, Common.remove_duplicates (List.flatten effobjss))
    | _ -> assert false

and constrain_field se_env (env, guard, _) expr label_desc =
  let (recframe, cstrs, effobjs) = constrain [] se_env expr env guard in
  let (fieldframe, fieldname) = match recframe with
    | F.Frecord (_, fs, _) -> (match List.nth fs label_desc.lbl_pos with (fr, name, _) -> (fr, name))
    | _ -> assert false
  in
  let pexpr = P.Field (fieldname, expression_to_pexpr expr) in
  let f = F.apply_refinement (B.equality_refinement pexpr) fieldframe in
  (f, [WFFrame (env, f)], cstrs, effobjs)

and constrain_if se_env (env, guard, f) e1 e2 e3 =
  let (f1, cstrs1, effobjs1) = constrain [] se_env e1 env guard in
  let guardvar = Path.mk_ident "guard" in
  let env' = Le.add guardvar f1 env in
	(* Add effect from the boolean condition *)
	let effect = Frame.eff f1 in
	let env' = Frame.update_env env' effect in
  let guard2 = (guardvar, true)::guard in
  let (f2, cstrs2, effobjs2) = constrain [] se_env e2 env' guard2 in
  let guard3 = (guardvar, false)::guard in
  let (f3, cstrs3, effobjs3) = constrain [] se_env e3 env' guard3 in
	(*let _ = Format.fprintf Format.std_formatter "if-then-else effect on: @." in
	let _ = List.iter (fun effobj -> Format.fprintf Format.std_formatter "%s " (Path.name effobj)) 
					(Common.remove_duplicates (effobjs1@effobjs2@effobjs3)) in*)
	let f = Frame.push_effect e1.exp_env env f (Common.remove_duplicates (effobjs1@effobjs2@effobjs3)) in
	(f,
	[WFFrame(env, f); SubFrame(env', guard2, f2, f); SubFrame(env', guard3, f3, f)],
	cstrs1 @ cstrs2 @ cstrs3,
	Common.remove_duplicates (effobjs1@effobjs2@effobjs3))
				

and bind tenv env guard pat frame pexpr =
	(** Important: Add effects into environment *)
	let effect = Frame.eff frame in
	let env = Frame.update_env env effect in
	let frame = Frame.ty frame in
  let env = F.env_bind tenv env pat.pat_desc frame in
	if Pattern.is_deep pat.pat_desc then
	  Le.add (Path.mk_ident "pattern")
	    (B.mk_int [(Path.mk_ident "", Path.mk_ident "", Pattern.desugar_bind pat.pat_desc pexpr)])
	    env
	else env 
	
and update_measures	se_env env pat matche matchf e = 
	let rec bind_rec env (pat, matche, matchf) =
    match pat with
    | Tpat_any -> env
    | Tpat_var x -> env
    | Tpat_tuple pats ->
      let pexps = Misc.mapi (fun pat i -> 
				match matchf with
					| Frame.Frecord (_, ts, _) ->
						let (matchf,_,_) = List.nth ts i in
						(pat.pat_desc, P.Proj(i, matche), matchf)
					| Frame.Ftuple (ts, _) ->
						let matchf = List.nth ts i in
						(pat.pat_desc, P.Proj(i, matche), matchf)
					| _ -> assert false	) pats in
      List.fold_left bind_rec env pexps
		| Tpat_construct (constructor_desc, patterns) ->
			constr_bind se_env env matche matchf e constructor_desc patterns 
		| pdes -> env
  in bind_rec env (pat, matche, matchf) 
	
and constr_bind se_env env matche matchf e constructor_desc patterns = 
	(match constructor_desc.cstr_res.desc with
		| Tconstr(p, args, _) when Path.same p Predef.path_list ->
			if (List.length patterns = 0) then 
				let e = Predicate.Atom (Predicate.FunApp ("List.length", [matche]), Predicate.Eq, Predicate.PInt 0) in
				let e = 
				if !(Clflags.reachability) then 
					let (b, p) = Datatype.extract_value_type_from_container_fr matchf in
					let sublinks = if b then [] else Datatype.get_all_links udt_table p in
					let link_refinement1 = 
						Predicate.logic_equals (Predicate.Reach (matche,forall_uexpr))
						(Predicate.Not Predicate.True) in
					let link_refinement2 = 
						if b then
							Predicate.logic_equals (Predicate.Link (matche,"cons",1,forall_uexpr,forall_vexpr))
							(Predicate.Not Predicate.True) 
						else 
							Predicate.big_and (List.map (fun (n, i) -> 
								let n = String.lowercase_ascii n in
								Predicate.logic_equals (Predicate.Link (matche,n,i,forall_uexpr,forall_vexpr))
								(Predicate.Not Predicate.True) 
							) sublinks) in	
					let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
					let link_refinement2 = Predicate.Forall (foralls, link_refinement2) in
					Predicate.big_and [link_refinement1; link_refinement2; e]
				else e in
				Le.add (Path.mk_ident "lstlenencoding")
				(B.mk_int [(Path.mk_ident "", Path.mk_ident "", e)])
				env
			else 
				let tl = List.nth patterns 1 in
				let tl = match tl.pat_desc with Tpat_var id -> Path.Pident id | _ -> Path.mk_ident "tl" in
				let tl = Predicate.Var tl in
				let e = Predicate.And 
					(Predicate.Atom (Predicate.FunApp ("List.length", [matche]), Predicate.Eq, 
					Predicate.Binop (Predicate.PInt 1, Predicate.Plus, Predicate.FunApp ("List.length", [tl]))),
					Predicate.Atom (Predicate.FunApp ("List.length", [tl]), Predicate.Ge, Predicate.PInt 0)) in
				let e = 
					if !(Clflags.reachability) then 
						let (b, p) = Datatype.extract_value_type_from_container_fr matchf in
						let sublinks = if b then [] else Datatype.get_all_links udt_table p in
						let hd = match (List.nth patterns 0).pat_desc with
							| Tpat_var id -> Predicate.Var (Path.Pident id)
							| _ -> (Format.fprintf Format.std_formatter "internal bug@."; assert false) in
						let link_refinement1 = 
							if b then
								Predicate.logic_equals (Predicate.Reach (matche, forall_uexpr))
									(Predicate.Or (Predicate.Atom (hd,Predicate.Eq,forall_uexpr), Predicate.Reach (tl, forall_uexpr))
								) 
							else 
								Predicate.logic_equals (Predicate.Reach (matche, forall_uexpr))
								(Predicate.Or (Predicate.Reach (hd,forall_uexpr), Predicate.Reach (tl, forall_uexpr))
								) in
						let link_refinement2 = 
							if b then
								Predicate.logic_equals (Predicate.Link (matche,"cons",1,forall_uexpr,forall_vexpr))
									(Predicate.Or ( 
										Predicate.And (Predicate.Atom (hd,Predicate.Eq,forall_uexpr), Predicate.Reach (tl, forall_vexpr)),
										Predicate.Link (tl,"cons",1,forall_uexpr,forall_vexpr))
								)
							else 
								Predicate.big_and (List.map (fun (n, i) -> 
									let n = String.lowercase_ascii n in
									Predicate.logic_equals (Predicate.Link (matche,n,i,forall_uexpr,forall_vexpr))
									(Predicate.Or ( 
										Predicate.Link (hd,n,i,forall_uexpr,forall_vexpr),
										Predicate.Link (tl,n,i,forall_uexpr,forall_vexpr))
									)
								) sublinks) in	
						let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
						let link_refinement2 = Predicate.Forall (foralls, link_refinement2) in
						Predicate.big_and [link_refinement1; link_refinement2; e]
					else e in		
				Le.add (Path.mk_ident "lstlenencoding")
				(B.mk_int [(Path.mk_ident "", Path.mk_ident "", e)])
				env
		| Tconstr(p, _, _) when (Hashtbl.mem se_env.measures p) ->
			let cstrname = constructor_desc.cstr_name in
			(*let args = List.map (fun pattern -> match pattern.pat_desc with
				| Tpat_var id -> Predicate.Var (Path.Pident id) 
				| Tpat_construct (constructor_desc, patterns) -> 
				| _ -> assert false
				) patterns in*)
			let (env, args) = List.fold_left (fun (env, args) pattern -> 
				match pattern.pat_desc with
				| Tpat_var id -> (env, args @ [Predicate.Var (Path.Pident id)])
				| Tpat_construct (constructor_desc, patterns) -> 
					let matche' = (Predicate.Var (Path.mk_ident "dtystub")) in
					let env = constr_bind se_env env matche' matchf e constructor_desc patterns in
					(env, args @ [matche'])
				| _ -> assert false
				) (env, []) patterns in
			let decl = Env.find_type p e.exp_env in
			let varis = List.map Frame.translate_variance decl.type_variance in
			Le.add (Path.mk_ident "dtyencoding") (
				Frame.Fconstr(p, [], varis, ([], Frame.Qconst [(Path.mk_ident "", Path.mk_ident "", 
				Predicate.Atom (matche, Predicate.Eq, 
					Predicate.FunApp (cstrname, args))
				)]), []))	env
		| tpat -> env)

(** If matche produces side effect then casee should also produces the same effect *)
and constrain_case se_env (env, guard, f) matchf matche (pat, e) =
  let env = bind e.exp_env env guard pat matchf matche in
	(** To encode a simple sort here --> mapping a list to its length  *)
	(*let env = match pat.pat_desc with in*)
	let env = update_measures se_env env pat.pat_desc matche matchf e in	
	(*let f = Frame.push_effect env f effobjs in*)
  let (fe, subcs, effobjs) = constrain [] se_env e env guard in
  (*(SubFrame (env, guard, fe, f), subcs)*)
	((env, fe), subcs, effobjs)

and constrain_match se_env ((env, guard, f) as environment) e pexps partial =
  let (matchf, matchcstrs, effobjs) = constrain [] se_env e env guard in
  let cases = List.map (constrain_case se_env environment matchf (expression_to_pexpr e)) pexps in
  (*let (cstrs, subcstrs) = List.split cases in 
  (f, WFFrame (env, f) :: cstrs, List.concat (matchcstrs :: subcstrs))*)
	let (fes, subcstrs, effobjss) = Misc.split3 cases in
	let effobjs = Common.remove_duplicates (effobjs@(List.flatten effobjss)) in
	let f = Frame.push_effect e.exp_env env f effobjs in
	let cstrs = List.map (fun (env, fe) -> SubFrame (env, guard, fe, f)) fes in
	(f, WFFrame (env, f) :: cstrs, List.concat (matchcstrs :: subcstrs), effobjs)
	

(* For the piece of code e', the effect of e' should subtype to   *)
and constrain_function inherit_effect se_env (env, guard, f) t pat e' =
  match f with
    | F.Farrow (_, f, unlabelled_f', []) ->
      (let env' = F.env_bind e'.exp_env env pat.pat_desc f in
      let (f'', cstrs, effobjs) = constrain [] se_env e' env' guard in
			
			(*let _ = Format.fprintf Format.std_formatter "function body effect on: @." in*)
			let _ = List.iter (fun effobj -> Format.fprintf Format.std_formatter "%s " (Path.name effobj)) 
					(Common.remove_duplicates effobjs) in
			
			let f' = F.label_like unlabelled_f' f'' in
			
			(** The effect of f'' should flow into f' *)
			let f' = Frame.push_effect e'.exp_env env' f' effobjs in
			
			(*let _ = Format.fprintf Format.std_formatter "@.function body frame: %a@." Frame.pprint f' in*)
			
      let f = F.Farrow (Some pat.pat_desc, f, f', []) in
      (f, [WFFrame (env, f); SubFrame (env', guard, f'', f')], cstrs, effobjs))
    | _ -> assert false

and instantiate_id id f env tenv =
  let env_f =
    try Le.find id env
    with Not_found -> Frame.fresh_without_vars tenv ((Env.find_value id tenv).val_type) in
	(*let _ = Format.fprintf Format.std_formatter "env_f = %a@." Frame.pprint env_f in*)
  F.instantiate env_f f

and constrain_base_identifier (env, _, f) id e =
  let refn =
    if Le.mem id env then B.equality_refinement (expression_to_pexpr e) else F.empty_refinement
  in (F.apply_refinement refn (instantiate_id id f env e.exp_env), [], [], [])

and constrain_identifier (env, guard, f) id tenv =
  (*let _ = Format.fprintf Format.std_formatter "Func id frame = %a@." Frame.pprint f in*)
	let f = instantiate_id id f env tenv in 
	(*let _ = Format.fprintf Format.std_formatter "Instantiated func id frame = %a@." Frame.pprint f in*)
	(f, [WFFrame(env, f)], [], [])	

and apply_once se_env env guard (f, cstrs, subexp_cstrs, index, effobjs) e = 
	match (f, e) with
  | (F.Farrow (l, f, f', []), (Some e2, _)) ->
		(*let _ = Format.fprintf Format.std_formatter "param=%a@." Predicate.pprint_pexpr (expression_to_pexpr e2) in *)
    let (f2, e2_cstrs, effobjs') = constrain [] se_env e2 env guard in
		let effect = Frame.eff f2 in
		let env = Frame.update_env env effect in
		let f2 = Frame.ty f2 in
    let f'' = match l with
      | Some pat ->
        List.fold_right F.apply_substitution (Pattern.bind_pexpr pat (expression_to_pexpr e2)) f'
          (* pmr: The soundness of this next line is suspect,
             must investigate (i.e., what if there's a var that might
             somehow be substituted that isn't because of this?  How
             does it interact w/ the None label rules for subtyping?) *)
      | _ -> (
				(*let _ = Format.fprintf Format.std_formatter "hidden_p=%s and hidden_e=%a@." 
					(Path.name (F.get_ho_param index)) Predicate.pprint_pexpr (expression_to_pexpr e2) in*)
				F.apply_substitution (F.get_ho_param index, (expression_to_pexpr e2)) f'
				)
    in (f'', SubFrame (env, guard, f2, f) :: cstrs, e2_cstrs @ subexp_cstrs, index+1, effobjs@effobjs')
  | _ -> assert false

and constrain_application se_env (env, guard, _) func exps =
  let (func_frame, func_cstrs, effobjs) = constrain [] se_env func env guard in
	(*let _ = Format.fprintf Format.std_formatter "Func frame = %a@." Frame.pprint func_frame in
	let _ = Format.fprintf Format.std_formatter "start for parameters@." in*)
	let (app_f, app_cstrs, app_rec_cstrs, _, effobjs') = 
		List.fold_left (apply_once se_env env guard) (func_frame, [], func_cstrs, 0, []) exps in
	(*let _ = Format.fprintf Format.std_formatter "end for parameters@." in*)
  (* From the func_frame, if this application is partial, then push additional substituion *)
	let countables = Frame.count_named_args func_frame in
	let n_supplied = abs (List.length exps - countables) in
	let gost_subs = Frame.get_partial_app_subs app_f n_supplied 0 0 in
	(*let _ = List.iter (fun (p, e) -> Format.fprintf Format.std_formatter "gost_p=%s and gost_e=%a@." (Path.name p) Predicate.pprint_pexpr e) gost_subs in*)
	(* Fixme for cases when partial application is called in the same context *)
	(* Also, should fix if the app_f dose not include enough effects *)
	let now_effect = List.map fst (Frame.eff app_f) in
	(*let _ = Format.fprintf Format.std_formatter "Now_effect:%a@." Frame.pprint_effect (Frame.eff app_f) in*)
	let _ = assert (List.for_all (fun o -> (List.exists (fun n -> Path.same o n) now_effect)) (effobjs@effobjs')) in
	(*let _ = List.iter (fun p -> Format.fprintf Format.std_formatter "effobj:%s@." (Path.unique_name p)) effobjs in
	let _ = List.iter (fun (p, fr) -> Format.fprintf Format.std_formatter "now_eff:%s@." (Path.unique_name p)) now_effect in
	if (List.exists (fun cp -> List.for_all (fun (np, _) -> not (Path.same cp np)
		) now_effect) effobjs) then assert false
	else*)
	(List.fold_right F.apply_substitution gost_subs app_f, app_cstrs, app_rec_cstrs, now_effect)
	

and constrain_let se_env (env, guard, f) recflag bindings body =
  let (env', cstrs1, effobjs) = constrain_bindings se_env env guard recflag bindings in
	(** Add higher order encoding to support local reasoning *)	
	let env' = encode_higher_order_function se_env env' recflag bindings in
  let (body_frame, cstrs2, effobjs') = constrain [] se_env body env' guard in
  match body.exp_desc with
    | Texp_let _ -> (body_frame, [WFFrame (env, body_frame)], cstrs1 @ cstrs2, effobjs@effobjs')
    | _ ->
			let f = F.label_like f body_frame in
			let f = Frame.push_effect body.exp_env env f (Common.remove_duplicates (effobjs@effobjs')) in
      (f, [WFFrame (env, f); SubFrame (env', guard, body_frame, f)], cstrs1 @ cstrs2, 
				Common.remove_duplicates (effobjs@effobjs'))

and constrain_array se_env (env, guard, f) elements =
	let sorted elements = 
		if (List.length elements < 2) then true
		else 
			fst (List.fold_left (fun (res, laste) e -> 
				if (res) then 
					let (laste', e') = (expression_to_pexpr laste, expression_to_pexpr e) in
					match (laste', e') with
						| (Predicate.PInt laste', Predicate.PInt e') -> ((laste' <= e'), e)
						| _ -> (false, e)
				else (res, e)
			) (true, List.hd elements) (List.tl elements)) in
  let (f, fs) =
    (match f with
      | F.Fconstr(p, l, varis, _, eff) -> 
				if (sorted (elements)) then
					(F.Fconstr(p, l, varis, B.size_lit_sorted_refinement(List.length elements), eff), l)
				else
					(F.Fconstr(p, l, varis, B.size_lit_refinement(List.length elements), eff), l)
      | _ -> assert false) in
  let list_rec (fs, c, effobjss) e = 
		(fun (f, cs, effobj) -> (f::fs, cs @ c, effobj::effobjss)) (constrain [] se_env e env guard) in
  let (fs', sub_cs, effobjss) = List.fold_left list_rec ([], [], []) elements in
	let effobjs = Common.remove_duplicates (List.flatten effobjss) in
	let fs = List.map (fun fr -> Frame.push_effect (List.hd elements).exp_env env fr effobjs) fs in
  let mksub b a = SubFrame(env, guard, a, b) in
    (f, WFFrame(env, f) :: List.map (mksub (List.hd fs)) fs', sub_cs, effobjs)

and constrain_sequence se_env (env, guard, _) e1 e2 =
  let (fc, cs1, effobjs) = constrain [] se_env e1 env guard in
	(** Important add effect into environments *)
	let effect = Frame.eff fc in
	let env = Frame.update_env env effect in
  let (f, cs2, effobjs') = constrain [] se_env e2 env guard in 
	(* Fixme. Ensure effobjs' include effobjs *)
	(assert (List.for_all (fun p -> List.exists (fun p' -> Path.same p p') effobjs') effobjs);
	(f, [], cs1 @ cs2, effobjs'))

and constrain_tuple se_env (env, guard, f) es =
  let (fs, subexp_cs, effobjss) = constrain_subexprs se_env env guard es in
  match f with
    | F.Ftuple (fresh_fs, _) ->
				let fresh_fs = 
					List.map2 (fun fresh_frame effobjs -> 
						Frame.push_effect (List.hd es).exp_env env fresh_frame effobjs) fresh_fs effobjss in
        let new_cs = List.fold_left2
          (fun cs rec_frame fresh_frame ->
            WFFrame (env, fresh_frame) :: SubFrame (env, guard, rec_frame, fresh_frame) :: cs)
          [] fs fresh_fs in
        let elem_qualifier fexpr n = B.proj_eq_qualifier n (expression_to_pexpr fexpr) in
          (F.Ftuple (fresh_fs, ([], F.Qconst (Misc.mapi elem_qualifier es))),
           WFFrame (env, f) :: new_cs, subexp_cs, Common.remove_duplicates (List.flatten effobjss))
    | _ -> assert false

and constrain_assertfalse (env, guard, f) =
	let assert_qualifier =
    (Path.mk_ident "assertfalse",
     Path.mk_ident "null",
     P.Not P.True)
  in (f, [SubFrame (env, guard, B.mk_int [], B.mk_int [assert_qualifier])], [], [])

and constrain_assert se_env (env, guard, _) e =
  let (f, cstrs, effobjs) = constrain [] se_env e env guard in
	let _ = assert (List.length effobjs = 0) in
  let guardvar = Path.mk_ident "assert_guard" in
  let env = Le.add guardvar f env in
  let assert_qualifier =
    (Path.mk_ident "assertion",
     Path.mk_ident "null",
     P.equals (B.tag(P.Var guardvar), P.int_true))
  in (B.mk_unit (), [SubFrame (env, guard, B.mk_int [], B.mk_int [assert_qualifier])], cstrs, effobjs)

and constrain_and_bind se_env guard (env, cstrs, effobjs) (pat, e) =
  let (f, cstrs', effobjs') = constrain [] se_env e env guard in
	(** bind functional body and frame for symbolic execution use *)
	let _ = se_env_bind_fun env se_env false pat e f in
  let env = bind e.exp_env env guard pat f (expression_to_pexpr e) in
  (env, cstrs @ cstrs', Common.remove_duplicates (effobjs@effobjs'))

and bind_all se_env bindings fs tenv env guard =
  List.fold_right2 (fun (p, e, px) f env -> 
		(** bind functional body and frame for symbolic execution use *)
		let _ = se_env_bind_fun env se_env true p e f in
		bind tenv env guard p f px) bindings fs env

and constrain_bindings se_env env guard recflag bindings =
  match recflag with
  | Default | Nonrecursive -> List.fold_left (constrain_and_bind se_env guard) (env, [], []) bindings
  | Recursive ->
    let tenv = (snd (List.hd bindings)).exp_env in
    let (_, exprs) = List.split bindings in
    let bindings = List.map (fun (p, e) -> (p, e, expression_to_pexpr e)) bindings in

    (* We need to figure out all the frames' labels before we can properly bind them. *)
    let unlabeled_frames = List.map (fun e -> F.fresh e.exp_env e.exp_type) exprs in
    let unlabeled_env = bind_all se_env bindings unlabeled_frames tenv env guard in
    let (label_frames, _, effobjs) = constrain_subexprs se_env unlabeled_env guard exprs in
    let binding_frames = List.map2 F.label_like unlabeled_frames label_frames in
		let binding_frames = List.map2 (fun fr effobj -> 
			Frame.push_effect tenv unlabeled_env fr effobj) binding_frames effobjs in
            
    (* Redo constraints now that we know what the right labels are *)
    let bound_env = bind_all se_env bindings binding_frames tenv env guard in
    let (found_frames, subexp_cstrs, effobjs) = constrain_subexprs se_env bound_env guard exprs in

    let make_cstr fc = {lc_cstr = fc; lc_tenv = tenv; lc_orig = Loc Location.none; lc_id = fresh_fc_id ()} in
    let build_found_frame_cstr_list cs found_frame binding_frame =
      make_cstr (WFFrame (bound_env, binding_frame)) ::
      make_cstr (SubFrame (bound_env, guard, found_frame, binding_frame)) :: cs
    in (bound_env, (List.fold_left2 build_found_frame_cstr_list [] found_frames binding_frames) @ subexp_cstrs, 
				Common.remove_duplicates (List.flatten effobjs))

and constrain_subexprs se_env env guard es =
  List.fold_right (fun e (fs, cs, effobjs) -> 
		let (f, cs', effobjs') = constrain [] se_env e env guard in 
		(f :: fs, cs' @ cs, effobjs'::effobjs)) es ([], [], [])

let constrain_structure se_env initfenv initquals str =
  let rec constrain_rec quals fenv cstrs = function
    | [] -> (quals, fenv, cstrs)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs',_) = constrain [] se_env exp fenv []
        in constrain_rec quals fenv (cstrs' @ cstrs) srem
    | (Tstr_qualifier (name, (valu, pred))) :: srem ->
        let quals = (Path.Pident name, Path.Pident valu, pred) :: quals in
          constrain_rec quals fenv cstrs srem
    | (Tstr_value (recflag, bindings))::srem ->
        let (fenv, cstrs',_) = constrain_bindings se_env fenv [] recflag bindings
        in constrain_rec quals fenv (cstrs @ cstrs') srem
    | (Tstr_type tylst)::srem ->
				(List.iter (fun (id, declaration) -> (
					Format.fprintf Format.std_formatter "user-defined data-type %s@." (Ident.name id);
					Hashtbl.replace udt_table (Path.Pident id) declaration;
					Hashtbl.replace se_env.measures (Path.Pident id) [])) tylst;
        constrain_rec quals fenv cstrs srem)
    | _ -> assert false
  in constrain_rec initquals initfenv [] str

module QualifierSet = Set.Make(Qualifier)

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from the environment. *)
let instantiate_in_environments cs qs =
  let domains = List.map (fun c -> match c.lc_cstr with SubFrame (e,_,_,_) | WFFrame (e,_) -> Lightenv.domain e) cs in
  let instantiate_qual qualset q =
    let instantiate_in_env qset d =
      let varmap = Common.map_partial (fun path -> match Path.ident_name path with Some name -> Some (name, path) | None -> None) d in
      (* Must not instantiate the higher order function parameter variable *) 
			let varmap = 
				List.map (fun valu -> (Path.ident_name_crash valu, valu)) (F.get_all_ho_params () @ F.get_all_ex_params () @ [F.returnpath]) @ varmap in
			(* Search the data types 
			let tymap = List.map (fun p -> Path.ident_name_crash p, p) datatypes in*)	
			let inv = Qualifier.instantiate varmap q in
        match inv with
            Some inv -> QualifierSet.add inv qset
          | None -> qset
    in List.fold_left instantiate_in_env qualset domains
  in QualifierSet.elements (List.fold_left instantiate_qual QualifierSet.empty qs)

let make_frame_error s cstr =
  let rec error_rec cstr =
    match cstr.lc_orig with
      | Loc loc ->
        begin match cstr.lc_cstr with
          | SubFrame (_, _, f1, f2) ->
            (loc, NotSubtype (F.apply_solution s f1,  F.apply_solution s f2))
          | WFFrame (_,f) -> (loc, IllFormed (F.apply_solution s f))
        end
      | Assert loc -> (loc, AssertMayFail)
      | Cstr cstr -> error_rec cstr
  in error_rec cstr

let report_error ppf  = function
  | AssertMayFail ->
    fprintf ppf "@[Assertion may fail@]"
  | NotSubtype (f1, f2) ->
    fprintf ppf "@[@[%a@]@;<1 2>is not a subtype of@;<1 2>@[%a@]" F.pprint f1 F.pprint f2
  | IllFormed f ->
    fprintf ppf "@[Type %a is ill-formed" F.pprint f

let rec report_errors ppf = function
  | (l, e) :: es ->
    fprintf ppf "@[%a%a@\n@\n@]" Location.print l report_error e; report_errors ppf es
  | [] -> ()

let pre_solve () = 
  C.cprintf C.ol_solve_master "@[##solve##@\n@]"; Bstats.reset ()

let post_solve () = 
  if C.ck_olev C.ol_timing then
    (Printf.printf "##time##\n"; Bstats.print stdout "\nTime to solve constraints:\n";
    Printf.printf "##endtime##\n"; (*TheoremProver.dump_simple_stats ()*))

let lbl_dummy_cstr env c =
  { lc_cstr = c; lc_tenv = env; lc_orig = Loc (Location.none); lc_id = fresh_fc_id () }

let mfm fenv p f = 
  if Le.mem p fenv
  then
    let f' = Le.find p fenv in
    Some (SubFrame (fenv, [], f', F.label_like f f'))
  else
    None 

let return_repr = "r" 
				
(** Find all higher order fun parameters in invariant and subsitute them 
		with the fixed set of higher order fun parameters defined above *)		
(* Fixme? Hard coded. *)	
(** candidates is the (path: fr) binding for function definition 
  * tbl is the (string, path) binding for what the learning algorithm knows
	*)	
let normalize measures invariant candidates tbl target_repr return_repr = 
	let rec make_subs index p f res = match f with
		| Frame.Farrow (_,f1,f2,_) ->
			(** 1) From candidate p, get its name 2) From name, get invariant's path *)
			(let p_param_repr = (Path.name p) ^ "_" ^ (string_of_int index) in
			try let p_param = Hashtbl.find tbl p_param_repr
				(*(Hashtbl.iter (fun s p -> Format.fprintf Format.std_formatter "tbl %s:%s@." s (Path.name p)) tbl;
				Format.fprintf Format.std_formatter "p_param: %s@." p_param_repr;
				Format.fprintf Format.std_formatter "invariant: %a@." Predicate.pprint invariant;
				assert false)*) in
			let res = res @ [(p_param, Predicate.Var (F.get_ho_param index))] in
			make_subs (index+1) p f2 res 
			with Not_found -> (* The following line seems buggy but is actually provding arguments for encoding *)
				(make_subs (index+1) p f2 (res@[(p, Predicate.Var (F.get_ho_param index))])))
		| _ -> 
			(let p_return_repr = (Path.name p) ^ "_" ^ return_repr in
			try let p_return = Hashtbl.find tbl p_return_repr in
			(** Consdier also specially encode the parameters *)
			let encode_res = Misc.mapi (fun (param, pv) i -> (param, Predicate.FunApp (
				"Arg2", (** 2 means the number of arguments to this FunApp structure *)
				[(Predicate.Var p); (*pv*)Predicate.PInt i]
				))) res in
			let args = List.map snd encode_res in
			let encode_res = 
				if (String.compare (Path.name p) target_repr = 0) 
				then (* No encoding for target function parameter *) res else encode_res in
			encode_res @ [(p_return, Predicate.FunApp (
					"Ret"^(string_of_int (1 + List.length args)), 
					(Predicate.Var p)::args))] with Not_found -> (res)) in
	let make_arr_subs p f = 
		let inv_vars = Predicate.vars invariant in match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
			(* Fixme. But the implementation only works for one dimension array *)
			let p_param_repr = (Path.name p) ^ "_i" in 
			let ex_p_param_repr = "ex_" ^ (Path.name p) ^ "_i" in 
			let universal_subs = (try 
				let p_param = List.find (fun iv -> String.compare p_param_repr (Path.name iv) = 0) inv_vars in
				if (String.compare (Path.name p) target_repr = 0) then
					[(p_param, Predicate.Var (F.get_ho_param 0))]
				else (*Fix me?*)
					[(p_param, Predicate.Var (F.get_ho_param 0))]
					(*[(p_param, Predicate.FunApp ("Arg2", [(Predicate.Var p); Predicate.PInt 0]))]*)
			with _ -> (*The invariant has nothing to do with this array*) []) in
			let existential_subs = (try
				let ex_p_param = List.find (fun iv -> String.compare ex_p_param_repr (Path.name iv) = 0) inv_vars in
				if (String.compare (Path.name p) target_repr = 0) then
					[(ex_p_param, Predicate.Var (F.get_ex_param 0))]
				else assert false
			with _ -> []) in
			universal_subs @ existential_subs
		| _ -> assert false in
	let make_list_subs p f = 
		let inv_vars = Predicate.vars invariant in 
		match f with
			| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list ->
				let repr = (Path.name p) ^ "_l" in 
				(try
					let params = List.find_all (fun iv -> String.compare repr (Path.name iv) = 0) inv_vars in
					List.map (fun param -> (param, Predicate.FunApp ("List.length", [(Predicate.Var p)]))) params 
				with _ -> [])
			| _ -> assert false	in
	let make_measure_subs measures p f = 
		let inv_vars = Predicate.vars invariant in
		match f with
			| Frame.Fconstr (x,_,_,_,_) when (Hashtbl.mem measures x) ->
				let measures = Hashtbl.find measures x in
				List.fold_left (fun res (measure, _) -> 
					let m = Path.name measure in
					let repr = (Path.name p) ^ "_" ^ m in
					try 
						let params = List.find_all (fun iv -> String.compare repr (Path.name iv) = 0) inv_vars in	
						res @ (List.map (fun param -> (param, Predicate.FunApp (m, [(Predicate.Var p)]))) params)
					with _ -> res	
				) [] measures
			| _ -> [] in
	let subs = List.fold_left (fun res (p, f) -> match f with
		| Frame.Farrow (None,_,_,_) -> (** Depend upon what f looks like, make subs *)
			let subs = make_subs 0 p f [] in res@subs
		| Frame.Farrow (Some _,_,_,_) -> assert false
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> res@(make_arr_subs p f)
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list -> res@(make_list_subs p f)
		| Frame.Fconstr (x,_,_,_,_) when Hashtbl.mem measures x -> res@(make_measure_subs measures p f)
		| _ -> res
		) [] candidates in
	(*let _ = Format.fprintf Format.std_formatter "dump subs@." in
	let _ = List.iter (fun (path, pexp) -> 
		Format.fprintf Format.std_formatter "%s:%a@." (Path.name path) Predicate.pprint_pexpr pexp;
		) subs in*)
	Predicate.apply_substs subs invariant

(** For generating the inferred qualifiers *)	
let valu = Path.mk_ident "valu"
let pvalu = Predicate.Var valu

let rec find_params_return env frame res return_repr= match frame with
	| Frame.Farrow (Some pat, f1, f2, _) -> 
		let res = ((Frame.bind env pat f1)) @ res in
		(find_params_return env f2 res return_repr)
	| _ -> (Frame.returnpath(*Path.mk_ident return_repr*), frame) :: res 

let make_plain_subs env funframe tbl = 
	let candidates = find_params_return env funframe [] return_repr in 
	List.fold_left (fun res (destp, destfr) -> 
		let destp_repr = Path.name destp in
		if (Hashtbl.mem tbl destp_repr) then
			let inv_var = Hashtbl.find tbl destp_repr in
			res@[(inv_var, Predicate.Var destp)]
		else res
	) [] candidates
	
(** To generate samples using the guidances of previously generated invariants *)
let standarlize_invariant env funframe unsounds tbl invariant bad_constraint =
	let subs = ref [] in
	let allbindings = Frame.get_fun_bindings env funframe in
	(* We want to care about for-all paired array property *)
	let arr_pair = Backwalker.detect_arr_adj_pattern bad_constraint allbindings unsounds in
	let _ = ignore (Predicate.map_expr (fun pexpr -> match pexpr with
		| Predicate.Var path -> 
			let var_repr = Path.name path in
			(if (Hashtbl.mem tbl var_repr) then
				let var_in_inv = Hashtbl.find tbl var_repr in 
				(subs := (var_in_inv, pexpr)::(!subs)) else ();
			pexpr)
		| Predicate.FunApp (fn, es) -> 
			let (fpath, fn) = match List.hd es with 
				| Predicate.Var fpath  -> (fpath, Path.name fpath)
				| _ -> assert false in
			let es = List.tl es in
			if (List.exists (fun u -> (Predicate.FunApp (fn, es)) = u) unsounds) then pexpr
			(* if this funapp appears in array adj dection, specially encode it *) 
			else if (List.exists (fun (_, pe) -> pe = pexpr) arr_pair) then pexpr 
			else if (List.exists (fun (pe, _) -> pe = pexpr) arr_pair) then
				(* dealing with paired array property *)
				let (_, pexpr') = List.find (fun (pe, _) -> pe = pexpr) arr_pair in
				let fun_return_repr = fn ^ "_" ^ return_repr in
				let _ = 
					if (Hashtbl.mem tbl fun_return_repr) then
						let fun_return_in_inv = Hashtbl.find tbl fun_return_repr in
						(subs := (fun_return_in_inv, pexpr)::(!subs)) in
				let _ = List.fold_left (fun i e -> 
					let fun_param_repr = fn ^ "_" ^ (string_of_int i) in
					(if (Hashtbl.mem tbl fun_param_repr) then
						let fun_param_repr_in_inv = Hashtbl.find tbl fun_param_repr in
						subs := (fun_param_repr_in_inv, e)::(!subs) else ();
					i+1)
					) 0 es in
				let fun_return_repr' = fn ^ "_" ^ return_repr ^ "'" in
				let _ = 
					if (Hashtbl.mem tbl fun_return_repr') then
						let fun_return_in_inv' = Hashtbl.find tbl fun_return_repr' in
						(subs := (fun_return_in_inv', pexpr')::(!subs)) in 		
				pexpr
			else (* Consider single array or function *)
				(try
					let (_,fr) = List.find (fun (p,f) -> Path.same p fpath) allbindings in
					let n_args = Frame.count_args fr in
					let n_actuals = List.length es in
					if (n_actuals >= n_args) then ( (** Full function application contributes to a sub *)
						let fun_return_repr = fn ^ "_" ^ return_repr in
						(*let _ = (
							Format.fprintf Format.std_formatter "fun_return_repr = %s@." fun_return_repr;
							Hashtbl.iter (fun k v -> 
								Format.fprintf Format.std_formatter "tbl k = %s v = %s@." k (Path.name v) 
								) tbl
							) in*)
						let _ = 
							if (Hashtbl.mem tbl fun_return_repr) then
								let fun_return_in_inv = Hashtbl.find tbl fun_return_repr in
								(subs := (fun_return_in_inv, pexpr)::(!subs)) in
						let _ = List.fold_left (fun i e -> 
							let fun_param_repr = fn ^ "_" ^ (string_of_int i) in
							(if (Hashtbl.mem tbl fun_param_repr) then
								let fun_param_repr_in_inv = Hashtbl.find tbl fun_param_repr in
								subs := (fun_param_repr_in_inv, e)::(!subs) else ();
							i+1)
							) 0 es in
						 pexpr
					)	
					else (** do not contribute to a subsitution *) pexpr
				with _ -> 
					( 
					(*assert false*)pexpr) (*(** Fixme. do not contribute to a subsitution *) pexpr*))
		| _ -> pexpr
		) bad_constraint)	in
	Predicate.apply_substs (!subs) invariant	

(** Rejects commons from bad formula *)
let reject_commons env unsounds commons funbadcondition funframe tbl return_repr =
	let commons = List.filter (fun common ->
		let vars = Common.remove_duplicates (Predicate.vars common) in
		List.for_all (fun var -> 
			let var_repr = Path.name var in
			if (String.length var_repr < String.length return_repr) then true
			else
				let var_repr_last = 
					String.sub var_repr 
					(String.length var_repr-String.length return_repr) 
					(String.length return_repr) in
				(String.compare var_repr_last return_repr != 0)
		) vars
	) commons in
	(*let subs = make_plain_subs env funframe tbl in*)
	(*let commons = List.map (Predicate.apply_substs subs) commons in*)
	let commons = 
		List.map (fun common -> (common, standarlize_invariant env funframe unsounds tbl common funbadcondition)) commons in 
	let commons = List.filter (fun (orig, common) -> 
		let result = TheoremProver.unsat (Predicate.And (funbadcondition, Predicate.Not common)) in
		(*let result = TheoremProver.implies (funbadcondition) common in*)
		((*TheoremProver.finish ();*) result)
		) commons in
  List.map fst commons
	
(* Translate r.x.y -->  Proj (y, Proj (x, r))*)
let decode_record var = 
	let name = Path.name var in
	let stamp = Path.stamp var in
	let names = Str.split (Str.regexp "\\.") name in	
	let (name, fields) = (List.hd names, List.tl names) in
	let res = Predicate.Var (Path.Pident (Ident.create_with_stamp name stamp)) in
	List.fold_left (fun res field -> (** Take care of if measure function is called on res *)
		let measures = Str.split (Str.regexp "_") field in
		let (field, measures) = (List.hd measures, List.tl measures) in
		let res = 
			try 
				let proj = int_of_string field in
				Predicate.Proj (proj, res)
			with _ -> Predicate.Field (field, res) in
		List.fold_left (fun res measure -> 
			if (String.compare measure "l" = 0) then
				Predicate.FunApp ("List.length", [res])
			else  (*Fixme. Not Supported*)
			  Predicate.FunApp (measure, [res])	
		) res measures
	) res fields
				
(** Translate the invariant to qualifiers; Probably the top level env suffices
 * to find constructor definition 
 * tbl is the (string, path) binding for what the learning algorithm knows
 * return_repr is how return value is represented in the implementation
 *) (* Fixme? Hard coded *)
let from_invariant b env measures invariant funpath funframe tbl return_repr commons = 
	let candidates = find_params_return env funframe [] return_repr in 
	(** Add commons into invariant: 
	 * If invariant refers to return_repr then commons without a_0 should definitely be added
	 * else if invariant refers to a_r or a_0 then commons with a_0 should definitely be added
	 *) (* Fixme: assume the form of parameters without "_" *)
	let commons = List.filter (fun common -> 
		let common_vars = Common.remove_duplicates (Predicate.vars common) in
		let inv_vars = Common.remove_duplicates (Predicate.vars invariant) in
		(List.exists (fun common_var -> 
			List.exists (fun inv_var -> 
				try let common_repr = 
					String.sub (Path.name common_var) 0 (String.rindex (Path.name common_var) '_') in
				let inv_repr = 
					String.sub (Path.name inv_var) 0 (String.rindex (Path.name inv_var) '_') in
				(String.compare common_repr inv_repr = 0) with _ -> false
			) inv_vars
		) common_vars) || 
		((List.exists (fun inv_var -> (String.compare (Path.name inv_var) return_repr = 0)
			) inv_vars) && (List.for_all (fun common_var-> 
				not (Common.str_contains (Path.name common_var) "_")
				) common_vars)) 
	) commons in
	let invariant = if (List.length commons = 0) then invariant
	else if not b then invariant
	else if (List.length commons > 0) then
		Predicate.Or (
			Predicate.big_and (invariant::commons), 
			Predicate.Not (Predicate.big_and commons))
	else invariant in
	(* Added *)
	(*let _ = Format.fprintf Format.std_formatter "Reach here@." in
	let _ = Hashtbl.iter (fun s p -> Format.fprintf Format.std_formatter "tbl s = %s p = %s@." s (Path.name p)) tbl in
	let _ = List.iter (fun (p, f) -> Format.fprintf Format.std_formatter "candidate=%s@." (Path.name p)) candidates in*)
	let inv_qualifier =  List.fold_left (fun res (destp, destfr) -> match res with
		| Some _ -> res
		| None -> (match destfr with
			| Frame.Farrow (None, _, _, _) -> (
				try (let destp_repr = ((Path.name destp) ^ "_" ^ return_repr) in
				let inv_var = Hashtbl.find tbl destp_repr in
				if (List.exists (fun iv -> Path.same inv_var iv) (Predicate.vars invariant)) then
					let invariant = Predicate.subst pvalu inv_var invariant in
					let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else raise Not_found)
				with Not_found -> (
					(** Considering if the invariant is for the fucntion argument *)
					let n = Frame.count_args destfr in
					let destp_reprs = Array.to_list (Array.init n (fun i -> i)) in
					List.fold_right (fun i res -> match res with
						| Some _ -> res
						| None -> (
								try (let destp_repr = ((Path.name destp) ^ "_" ^ (string_of_int i)) in
								let inv_var = Hashtbl.find tbl destp_repr in
								if (List.exists (fun iv -> Path.same inv_var iv) (Predicate.vars invariant)) then
									let invariant = Predicate.subst pvalu inv_var invariant in
									let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
									Some (F.get_refinement_variable_by_index destfr i, (funpath, valu, invariant))
								else None
								) with Not_found -> None)
					) destp_reprs None)
				(*try (let inv_var = (List.find (fun inv_var -> 
					let inv_var_str_repr = Path.name inv_var in
					let destp_repr = ((Path.name destp) ^ "_" ^ return_repr) in
					(String.compare destp_repr inv_var_str_repr)
					) inv_vars) (** check if the parameter show up in invariant *) in
				let invariant = Predicate.subst pvalu inv_var invariant in
				let invariant = normalize invariant candidates tbl return_repr in
				Some (qname, valu, invariant))
				with Not_found -> None*)
				)
			| Frame.Frecord _
			| Frame.Ftuple _ -> (
				let vars = Predicate.vars invariant in
				if (List.exists (fun var -> Path.same destp var) vars) then
					let invariant = Predicate.subst pvalu destp invariant in 
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else
				try (
				let repr = (Path.name destp) in
				let destp_repr = repr ^ "." in
				let inv_vars = Hashtbl.fold (fun repr var res -> 
					if (Common.str_contains repr destp_repr) then res @ [var] else res 	
				) tbl [] in
				(*let _ = List.iter (fun v -> Format.fprintf Format.std_formatter "%s@." (Path.name v)) inv_vars in*)
				if (List.exists (fun iv -> List.exists (fun inv_var -> 
					Path.same inv_var iv) inv_vars
					) (vars)) then		
					let substs = List.map (fun inv_var -> 
						(inv_var, decode_record inv_var)) inv_vars in
					(*let _ = List.iter (fun (p, e) -> 
						Format.fprintf Format.std_formatter "subs %s for %a@." (Path.unique_name p) Predicate.pprint_pexpr' e) substs in*)
					let invariant = Predicate.apply_substs substs invariant in
					let stampsubsts = List.map (fun inv_var -> 
						let stamp = Path.stamp inv_var in
						(Path.Pident (Ident.create_with_stamp repr stamp), pvalu)
					) inv_vars in
					let invariant = Predicate.apply_substs stampsubsts invariant in
					let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else None)
				with Not_found -> None)
			| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list -> (
				let vars = Predicate.vars invariant in
				if (List.exists (fun var -> Path.same destp var) vars) then
					let invariant = Predicate.subst pvalu destp invariant in 
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else
				try (let destp_repr = ((Path.name destp) ^ "_" ^ return_repr) in
				let inv_var = Hashtbl.find tbl destp_repr in
				if (List.exists (fun iv -> Path.same inv_var iv) (vars)) then
					let invariant = Predicate.subst pvalu inv_var invariant in
					let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else raise Not_found)
				with Not_found -> (
					(** Consider if the invariant is for list length *)
					try (let destp_repr = ((Path.name destp) ^ "_l") in
					let inv_var = Hashtbl.find tbl destp_repr in
					if (List.exists (fun iv -> Path.same inv_var iv) (vars)) then 
						let invariant = Predicate.subst (Predicate.FunApp ("List.length", [pvalu])) inv_var invariant in 
						let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
						Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
					else None)	
					with Not_found -> None
				))
			| Frame.Fconstr (x,_,_,_,_) when (Hashtbl.mem measures x) -> (
				let vars = Predicate.vars invariant in
				(* Translate invariant over itself *)
				if (List.exists (fun var -> Path.same destp var) vars) then
					let invariant = Predicate.subst pvalu destp invariant in 
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				(* Translate invariant over measures *)
				else
				let measures' = Hashtbl.find measures x in
				let (substs, tags) = List.fold_left (fun (ressubs, restags) var -> 
					let varname = Path.name var in
					let destname = Path.name destp in
					if (Common.str_contains varname destname && List.exists (fun (m, _) ->
								Common.str_contains varname ("_"^(Path.name m))) measures') then
						let names = Str.split (Str.regexp "_") varname in	
						let (varname, measure) = (List.hd names, List.hd (List.tl names)) in	 
						let names = Str.split (Str.regexp "\\.") varname in
						let (varname, fields) = (List.hd names, List.tl names) in
						if (String.compare varname destname = 0) then
							if (List.length fields = 0) then 
								((var, Predicate.FunApp (measure, [pvalu]))::ressubs, restags)
							else if (List.length fields = 2) then
								let proj = int_of_string (List.hd fields) in
								((var, Predicate.FunApp (measure, [Predicate.Proj (proj, pvalu)]))::ressubs, 
								(List.nth fields 1::restags))
							else assert false
						else (ressubs, restags) 
					else (ressubs, restags)	
				) ([], []) vars in
				if (List.length substs = 0) then None
				else if (List.length tags = 0) then 
					let invariant = Predicate.apply_substs substs invariant in
					let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else if (List.length tags = 1) then 
					let invariant = Predicate.implies (Predicate.Atom (Predicate.FunApp ("tag", 
						[Predicate.Var (Path.Pident (Ident.create_persistent (List.hd tags)))]), Predicate.Eq, Predicate.PInt 1), 
						Predicate.apply_substs substs invariant) in
					let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else assert false )
			| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> (	
				if (Hashtbl.length tbl = 0) then (* Translate the invaraints from the rulesampling **) 
					let allbindings = Frame.get_fun_bindings env funframe in
					(* Firstly the invariant should talk about destp *)
					let funcs = Predicate.get_all_funs invariant in
					let mentioned = List.exists (fun func -> match func with
						| Predicate.FunApp (arr, args) -> 
							String.compare arr (Path.name destp) = 0
						| _ -> false) funcs in 
					if (mentioned) then 
						(* Normalized for decidable checking *)
						let invariant = Rulemine.normalize invariant destp in
						(* Translate to "Ret" form *)
						let invariant = Predicate.map_expr (fun pexpr -> match pexpr with
							| Predicate.FunApp (arr, args) when (String.compare arr ("Array.length") <> 0) -> 
								if (Common.str_contains arr Frame.old_array_flag) then (* Old array *)
									let arr = String.sub (arr) (String.length (Frame.old_array_flag)) 
											(String.length (arr) - String.length (Frame.old_array_flag)) in	
									let (arrparam, arrfr) = 
										try List.find (fun (p, fr) -> String.compare (Path.name p) arr = 0) allbindings
										with _ -> assert false in
									let arrparam = Path.Pident (Ident.create_persistent (Frame.old_array_flag ^ (Path.name arrparam))) in
									(match arrfr with
										| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
											Predicate.FunApp ("Ret"^(string_of_int (1+List.length args)), (Predicate.Var arrparam)::args)
										| _ -> assert false)
								else (* Normal array *)
									let (arrparam, arrfr) = 
										try List.find (fun (p, fr) -> String.compare (Path.name p) arr = 0) allbindings
										with _ -> assert false in
									(match arrfr with
										| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
											Predicate.FunApp ("Ret"^(string_of_int (1+List.length args)), (Predicate.Var arrparam)::args)
										| _ -> assert false)
							| _ -> pexpr) invariant in
						(* Translate to qualifier *)
						let vars = Predicate.vars invariant in
						if (List.exists (fun iv -> Path.same destp iv) vars) then
							let invariant = Predicate.subst pvalu destp invariant in
							(** Variables old_a that indicate old array a is translated to old (a) as an uninterpreted function *)
							(*  And variables len_a that indicate array lenght is translated to Array.length (a) as uninterpreted function *)
						 	let invariant = (Predicate.map_expr (fun expr -> match expr with
								| Predicate.Var var when (Common.str_contains (Path.name var) Frame.old_array_flag) -> 
									let c = String.sub (Path.name var) (String.length Frame.old_array_flag) 
													(String.length (Path.name var) - String.length Frame.old_array_flag) in
									(try let var = List.find (fun var -> String.compare (Path.name var) c = 0) vars in
									Predicate.FunApp (Frame.old_array_fun, [Predicate.Var var]) with _ -> assert false)
								| Predicate.Var var when (Common.str_contains (Path.name var) "len_") -> 
									let c = String.sub (Path.name var) (String.length "len_") 
													(String.length (Path.name var) - String.length "len_") in
									(try let var = List.find (fun var -> String.compare (Path.name var) c = 0) vars in
									Predicate.FunApp ("Array.length", [Predicate.Var var]) with _ -> assert false)
								| expr -> expr
							) invariant) in
							let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
							Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
						else assert false
					else None	 
				else (
					(** Consider if the invariant is for array length *)
					let vars = Predicate.vars invariant in
					try (let destp_repr = ((Path.name destp) ^ "_l") in
					let inv_var = Hashtbl.find tbl destp_repr in
					if (List.exists (fun iv -> Path.same inv_var iv) (vars)) then 
						let invariant = Predicate.subst (Predicate.FunApp ("Array.length", [pvalu])) inv_var invariant in 
						let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
						Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
					else None)	
					with Not_found -> None
				))
			| _ -> 
				let vars = Predicate.vars invariant in
				(* Translate invariant over itself *)
				if (List.exists (fun var -> Path.same destp var) vars) then
					let invariant = Predicate.subst pvalu destp invariant in 
					Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
				else
					try (let destp_repr = Path.name destp in
					let inv_var = Hashtbl.find tbl destp_repr in
					if (List.exists (fun iv -> Path.same inv_var iv) (vars)) then
						let invariant = Predicate.subst pvalu inv_var invariant in
						let invariant = normalize measures invariant candidates tbl (Path.name destp) return_repr in
						Some (F.get_refinement_variable destfr, (funpath, valu, invariant))
					else None)
					with Not_found -> None
				(*try (let inv_var = (List.find (fun inv_var -> 
					let inv_var_str_repr = Path.name inv_var in
					let destp_repr = Path.name destp in
					(String.compare destp_repr inv_var_str_repr)
					) inv_vars) (** check if the parameter show up in invariant *) in
					let invariant = Predicate.subst pvalu destp invariant in
					let invariant = normalize invariant in
					Some (qname, valu, invariant))
				with Not_found -> None*)
			)
	) None candidates in
	match inv_qualifier with
		| Some (k, inv_q) -> ((k, inv_q))
		| None -> (Format.fprintf Format.std_formatter "Cannot deal with invariant %a@."
							Predicate.pprint invariant; assert false)
		(*(None, (Path.mk_ident "", Path.mk_ident "", invariant))*)

let rec permutation list =
	let rec extract acc n = function
	  | [] -> raise Not_found
	  | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
	in
	let extract_rand list len =
	  extract [] (Random.int len) list
	in
	let rec aux acc list len =
	  if len = 0 then acc else
	    let picked, rest = extract_rand list len in
	    aux (picked :: acc) rest (len-1)
	in
	aux [] list (List.length list)

(* The query of negative samples may depend upon positive samples *)	
let ease_bad_condition path allbindings badc pos_samples = 
	if !(Clflags.hoflag) then
		let store = Hashtbl.create 5 in
		let get_subs_from_store store = (
			Hashtbl.fold (fun p i res -> match (p, i) with
				| (Predicate.Var p, Some i) -> res @ [(p, Predicate.PInt (int_of_string i))]
				| _ -> res
			) store []	
		) in
		let badvars = Common.remove_duplicates (Predicate.vars badc.post) in
		let goodlocaldumps localdumps = 
			try let (pbadvars, envbadvars) = List.partition (fun badvar -> 
				List.mem_assoc badvar allbindings
			) badvars in
			let pvalues = Common.map_partial (fun dump -> 
				let name = Str.split (Str.regexp ":") dump in
				try let (name_name, name_value) = (List.hd name, List.nth name 1) in
				if not (String.contains name_value '#') && 
							List.exists (fun badvar -> String.compare name_name (Path.name badvar) = 0) badvars
				then Some (name_name, name_value)
				else None with _ -> None (* Fixme. Probably OK *)
			) (fst localdumps) in
			let envvalues = Common.map_partial (fun dump -> 
				let name = Str.split (Str.regexp ":") dump in
				try let (name_name, name_value) = (List.hd name, List.nth name 1) in
				if not (String.contains name_value '#') && 
							List.exists (fun badvar -> String.compare name_name (Path.name badvar) = 0) badvars
				then Some (name_name, name_value)
				else None with _ -> None (* Fixme. Probably OK *)
			) (snd localdumps) in
			let pms = List.fold_left (fun res pbadvar -> 
				try
					let (_, value) = List.find (fun (name, v) -> String.compare name (Path.name pbadvar) = 0) pvalues in
					if (Path.same pbadvar Frame.returnpath) then res
					else res @ [Predicate.Atom (Predicate.Var pbadvar, Predicate.Eq, Predicate.PInt (int_of_string value))	]
				with Not_found -> res
				) [] pbadvars in
			let envms = List.fold_left (fun res envbadvar -> 
				try
					let (_, value) = List.find (fun (name, v) -> String.compare name (Path.name envbadvar) = 0) envvalues in
					res @ [Predicate.Atom (Predicate.Var envbadvar, Predicate.Eq, Predicate.PInt (int_of_string value))	]
				with Not_found -> res
				) [] envbadvars in	
			let ms = pms @ envms in
			let _ = List.iter (fun pred -> match pred with
				| Predicate.Atom (Predicate.Var path, Predicate.Eq, Predicate.PInt v) -> 
					Hashtbl.replace store (Predicate.Var path) (Some (string_of_int v))
				| _ -> assert false
				) ms in
			let f = Predicate.big_and (badc.post::ms) in
			(*let _ = Format.fprintf Format.std_formatter "f = %a@." Predicate.pprint f in*)
			let r = TheoremProver.sat f in 
			(*let _ = Format.fprintf Format.std_formatter "checkr = %b@." r in*)
			r with _ -> (false) in
		let random_pos_sample () = 
			(** Pick a postive sample. Randomly? A function is called in a location more than once? *)
			(* 1. find all logs for path; 2. find the log collected from the non-recursive call site; *)
			let localdumpss = (Hashtbl.fold (fun lineno (name, dumpings) res ->
				if (String.compare name (Path.name path) = 0) then
					(*let _ = Format.fprintf Format.std_formatter "At line no %s with %d times@." lineno (List.length dumpings) in*)
					res @ [(*List.map fst*) dumpings]
				else res 
			) pos_samples []) in
			let min = List.fold_left (fun res dumplings -> 
				if (res > List.length dumplings) then List.length dumplings
				else res	
			) Pervasives.max_int localdumpss in
			let localdumpss = List.filter (fun dumplings -> 
				(List.length dumplings) = min
			) localdumpss in
		  let localdumpss = List.flatten localdumpss in
			(*let localdumpss = (Hashtbl.fold (fun _ (name, dumpings) res ->
				if (String.compare name (Path.name path) = 0) && (List.length dumpings = 1) then
					res @ ((*List.map fst*) dumpings)
				else res 
			) pos_samples []) in*)
			(if (List.length localdumpss <= 0) then raise NoRun;
			(*let _ = Format.fprintf Format.std_formatter "List.length localdumpss = %d@." (List.length localdumpss) in*)
			let localdumpss = permutation localdumpss in
			try List.find (fun localdumps -> 
				goodlocaldumps localdumps) localdumpss with _ ->
			List.nth localdumpss ( Random.int (List.length localdumpss))) in
		let localdumps = ref ([], []) in
		let readlog sequences b = (* b indicates where to retrieve. In env or in def?? *)
			(*let _ = List.iter (fun sequence -> Format.fprintf Format.std_formatter "seq=%s@." sequence) sequences in*)
			let _ = if (!localdumps = ([], [])) then try (localdumps := random_pos_sample ()) with NoRun -> raise NoRun in
			let localdumps = !localdumps in
			let localdumps = if (b) then fst localdumps else snd localdumps in
			(*let _ = List.iter (fun local -> Format.fprintf Format.std_formatter "local=%s@." local) localdumps in*)
			let key = List.hd sequences in
			let args = List.tl sequences in
			let values = Common.map_partial (fun dump -> 
				let name = Str.split (Str.regexp ":") dump in
				try let (name_name, name_value) = (List.hd name, List.nth name 1) in
				if (String.compare key name_name = 0) then Some (name_value)
				else None with _ -> None (* Fixme. Probably OK *)
			) localdumps in 
			List.fold_left (fun res value -> match res with
				| Some value -> ( Some value)
				| None -> (*Fixme. Not general*)
					if (List.length args = 0) then Some value
					else if (String.contains value '#') then
						let sub_locals = Str.split (Str.regexp ",") value in
						let sub_locals = List.map (fun sub_local -> 
							let sub_name = Str.split (Str.regexp "#") sub_local in
							(List.hd sub_name, List.nth sub_name 1)
							) sub_locals in
						let sub_locals = List.rev sub_locals in
						let (uf_r, uf_args) = (List.hd sub_locals, List.rev (List.tl sub_locals)) in
						if (List.length uf_args = List.length args) then
							if (List.for_all2 (fun (k, v) arg -> v = arg) uf_args args) then
								Some (snd uf_r)
							else None (* Do not find the right record *) 
						else assert false (* Do not match the record *)
					else assert false (* Cannot be reached *)
			) None values in
		let rec get_value b e = ((* b = true => read from def /\ b = false => read from env *)
			(*let _ = Format.fprintf Format.std_formatter "get_value=%a@." Predicate.pprint_pexpr e in*)
			if (Hashtbl.mem store e) then Hashtbl.find store e
			else 
				(*let _ = Format.fprintf Format.std_formatter "lookup from the logs@." in*)
				let value = match e with
					| Predicate.FunApp (fn, es) ->
						if (String.compare "UF" fn = 0) then
							let hd = List.hd es in match hd with
								| Predicate.Var hd -> 
									let es = Common.map_partial (get_value b) (List.tl es) in
									(try readlog ((Path.name hd)::(es)) b with NoRun -> None)
								| _ -> assert false
						else assert false
					| Predicate.Var p -> (try readlog [(Path.name p)] b with NoRun -> None)
					| _ -> assert false in
				(*let _ = Format.fprintf Format.std_formatter "Value = %s@." 
				(match value with Some value -> value | None -> "None") in*)
				(Hashtbl.replace store e value; value)
		) in
		let map_uf_to_val e = 
			match e with
			| Predicate.FunApp (fn, es) ->
				if (String.compare "UF" fn = 0) then
					match get_value true e with
						| Some value -> let value = int_of_string value in Predicate.PInt value 
						| None -> (
							Format.fprintf Format.std_formatter "Did not record %a in logc!!!@." Predicate.pprint_pexpr e; 
							Predicate.PInt 0) (* Did not record it in the logc!!! *)
				else e  
			| _ -> e in
		(** If a post-bad condition refers to variable out of scope, we may substitute it with a real value *)
		let instantiate_bad_condition bad = 
			(*let _ = Format.fprintf Format.std_formatter "&&&&&&bad=%a@." Predicate.pprint bad in*)
			let outofscope_vars = ref [] in
			let outofscope_ho_params = ref [] in
			let inscope_vars = Common.map_partial (fun (p, f) -> match f with
				| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_int && not (Path.same p Frame.returnpath) -> Some p
				| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list && not (Path.same p Frame.returnpath) ->
					(** retrieve list length only *)
					let stamp = Path.stamp p in
					let path = Path.Pident (Ident.create_with_stamp ((Path.name p)^"_l") stamp) in Some path 
				| Frame.Fvar _ when not (Path.same p Frame.returnpath)-> Some p
				| _ -> None) allbindings in
			(* If a variable is out-of-scope but used as parameter to ho function, do not need substitution *)
			(* However, if an out-of-scope variable is used as parameter, consider substitution for safe bad sample *)
			let _ = Predicate.map_expr_from_top (fun expr -> match expr with
				| Predicate.Var var ->
					if (List.for_all (fun (p, f) -> not (Path.same var p)) allbindings && (* Measures should be exclued *)
							(List.for_all (fun (p, f) -> 
								let name = List.hd (Str.split (Str.regexp "_") (Path.name var)) in
								let stamp = Path.stamp var in
								let var = Path.Pident (Ident.create_with_stamp name stamp) in
								not (Path.same var p)) allbindings) && (* Records should be exclued *)
							(List.for_all (fun (p, f) ->
								let name = List.hd (Str.split (Str.regexp "\\.") (Path.name var)) in
								let stamp = Path.stamp var in
								let var = Path.Pident (Ident.create_with_stamp name stamp) in
								not (Path.same var p)) allbindings)) then
						(outofscope_vars := var::(!outofscope_vars); expr)
					else expr
				| Predicate.FunApp _ -> 
					let vars = Predicate.exp_vars expr in
					(outofscope_ho_params := 
						(List.filter (fun var -> List.for_all (fun (p, _) -> 
							not (Path.same var p)) allbindings) vars)@(!outofscope_ho_params); expr)
				| _ -> expr
			) bad in
			let outofscope_vars = Common.remove_duplicates (List.filter (fun var -> 
				List.for_all (fun hp -> not (Path.same var hp)) (!outofscope_ho_params)
				) (!outofscope_vars)) in
			let _ = List.iter (fun var -> 
				try ignore(get_value false (Predicate.Var var)) with _ -> ()
			) outofscope_vars in
			(* subsititute inscope_vars and outofscope_vars with real samples *)
			if (List.length outofscope_vars > 0) then
				(List.iter (fun var -> 
					try ignore(get_value true (Predicate.Var var)) with _ -> ()) inscope_vars;
				let subs = get_subs_from_store store in
				let subspred = List.map (fun (p, pe) -> 
					Predicate.Atom (Predicate.Var p, Predicate.Eq, pe)	
				) subs in
				(*let _ = Format.fprintf Format.std_formatter "subspred = %a for %s@." Predicate.pprint (Predicate.big_and subspred) (Path.name path) in*)
				Predicate.big_and (bad::subspred))
			else bad in
		(** Now functional code *)	
		let pre = Predicate.map_expr (fun pexpr -> match pexpr with
			| Predicate.FunApp (fn, es) ->
				if (String.compare "UF" fn = 0) then 
					let hd = List.hd es in
					let es = List.map map_uf_to_val (List.tl es) in
					Predicate.FunApp (fn, hd::es)
				else pexpr
			| _ -> pexpr
		) badc.pre in
		let post = Predicate.map_expr (fun pexpr -> match pexpr with
			| Predicate.FunApp (fn, es) ->
				if (String.compare "UF" fn = 0) then 
					let hd = List.hd es in
					let es = List.map map_uf_to_val (List.tl es) in
					Predicate.FunApp (fn, hd::es)
				else pexpr
			| _ -> pexpr
		) badc.post in
		let subs = get_subs_from_store store in
		let subspred = List.map (fun (p, pe) -> 
			Predicate.Atom (Predicate.Var p, Predicate.Eq, pe)	
		) subs in
		{pre = Predicate.big_and ((Predicate.apply_substs subs pre)::subspred); 
		post = instantiate_bad_condition (* Now out-of-scope variable is not a worry *)
			(Predicate.big_and ((Predicate.apply_substs subs post)::subspred))
		}
	else badc

(** Relax list/data structure in bad condition *)
let destruct_list_access bad = 
	let f badc = Predicate.map_expr (fun expr -> match expr with
	| Predicate.FunApp (fn, args) when (
			String.compare fn "List.hd" = 0 || (*String.compare fn "List.tl" = 0 ||*) String.compare fn "List.nth" = 0) -> 
		let li = try Predicate.exp_var (List.hd args) with _ -> assert false in
		let stamp = Ident.stamp (Path.head li) in
		let lipath = Path.Pident (Ident.create_with_stamp ((Path.name li)^"_r") stamp) in
		Predicate.Var lipath
	| Predicate.FunApp (fn, args) when (String.compare fn "List.tl" = 0) -> 
		let li = try Predicate.exp_var (List.hd args) with _ -> assert false in
		Predicate.Var li
	| Predicate.FunApp (fn, args) when (String.compare fn "List.length" = 0) ->
		(** Deal with a simple sort which maps a list to an integer, represent list length *)
		let li = Predicate.exp_var (List.hd args) in
		let stamp = Path.stamp li in
		let lipath = Path.Pident (Ident.create_with_stamp ((Path.name li)^"_l") stamp) in
		Predicate.Var lipath
	| expr -> expr
	) badc in
	{pre = f bad.pre; post = f bad.post}
	
let destruct_record bad = 
	try let f badc = Predicate.map_expr (fun expr -> match expr with
		| Predicate.Proj (i, e) -> 
			let e = Predicate.exp_var e in
			Predicate.Var (Path.Pident (Ident.create_with_stamp ((Path.name e)^"."^(string_of_int i))
			(Path.stamp e))) 
		| Predicate.Field (str, e) -> 
			let e = Predicate.exp_var e in
			Predicate.Var (Path.Pident (Ident.create_with_stamp ((Path.name e)^"."^str)
			(Path.stamp e))) 
		| e -> e
		) badc in
	{pre = f bad.pre; post =f bad.post} with _ -> assert false
	
(** Need to split an invariant if both xs_r and xs_l is mentioned *)
let split_invariant env funframe inv = 
	let allbindings = Frame.get_fun_bindings env funframe in
	(* Fixme. This should be generalized to all data structures (not just list) *)
	let xss = List.fold_left (fun res (p, f) -> match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list -> res @ [p]
		| f -> res
	) [] allbindings in
	if (List.length xss = 0) then [inv]
	else 
		let invpreds = Predicate.split inv in
		let xstbl = Hashtbl.create (List.length xss) in
		let invpreds = List.fold_left (fun res invpred -> 
			let vars = Predicate.vars invpred in
			try 
				let xs = List.find (fun xs' -> 
					List.exists (fun var -> String.compare ((Path.name xs')^"_r") (Path.name var) = 0) vars) xss in
				let _ =  
					if (Hashtbl.mem xstbl xs) then Hashtbl.replace xstbl xs ((Hashtbl.find xstbl xs)@[invpred]) 
					else Hashtbl.replace xstbl xs [invpred] in 
				res
			with _ -> res @ [invpred]
		) [] invpreds in
		if (List.length invpreds = 0) then
			(Hashtbl.fold (fun xs preds res -> 
				res @ [Predicate.big_and preds]
			) xstbl [])
		else
			(Predicate.big_and invpreds) :: (Hashtbl.fold (fun xs preds res -> 
				res @ [Predicate.big_and preds]
			) xstbl [])

(* Fixme ... Bad implementation *)
let modify file newlines =
	let in' = open_in file in
  let lines = ref [] in
  begin
    try
      while true do
        let line = input_line in' in
        lines := line :: !lines
      done
    with End_of_file -> ()
  end;
  close_in in';
  let lines = List.rev (List.tl !lines) in
	let lines = lines in
  let out = open_out file in
  List.iter
    (fun line ->
       output_string out line;
       output_string out "\n")
    lines;
	List.iter 
		(fun line ->
       output_string out line)
    newlines;
  close_out out		

(* Iteratively collecting more good samples *)					
let pos_samples = Hashtbl.create 13
let query_pos_samples env se_env unsounds str instrumented_fname prev_failed_invariants = 	
	let prefails = Hashtbl.create (Hashtbl.length se_env.badbindings) in
	let _ = Hashtbl.iter (fun path invs ->
		let bad = 
			if !(se_env.dty) then {pre = Predicate.Not Predicate.True; post = Predicate.Not Predicate.True} 
			else (Hashtbl.find se_env.badbindings path) in	
		let allbindings = Frame.get_fun_bindings env (Hashtbl.find se_env.funframebindings path) in
		let bad = destruct_record bad in
		let bad = destruct_list_access bad in
		let bad = ease_bad_condition path allbindings bad pos_samples in
		let unsounds = Hashtbl.find unsounds path in
		let funframe = Hashtbl.find se_env.funframebindings path in 
		let preinvs = Common.map_partial (fun (b,tbl,(_,inv)) -> 
			if (b) then (* postcondition driven *)
				Some (standarlize_invariant env funframe unsounds tbl inv bad.post)
			else None
		) invs.ipre in
		let postinvs = Common.map_partial (fun (b,tbl,(_,inv)) -> 
			if (b) then (* postconidtion driven *)
				Some (standarlize_invariant env funframe unsounds tbl inv bad.post)
			else None
		) invs.ipost in 
		(Hashtbl.replace prefails path
		(preinvs, postinvs))
	) prev_failed_invariants in
	(* send the previously inferred invariants to backward analysis for guiding new tests *)
	let (b, solutions) = Backwalker.drive_new_test env se_env prefails str in
	if (b || Hashtbl.length pos_samples = 0) then (* Proceeds if new tests are available *)
		(* instrument the source code for injecting new tests *)
		(*let oc = open_out_gen [Open_text; Open_append] 0o640 instrumented_fname in
		let len = out_channel_length oc in
		let _ = Format.fprintf Format.std_formatter "ch len = %d" len in
		let pos = len - 25 in
		let buf = ("\nlet _ = " ^ (!Backwalker.main_function) ^ " " ^ solution) in
		let _ = seek_out oc pos in
		let _ = output_string oc ("\nlet _ = open_out_gen [Open_text; Open_append] 0o640 \"mllog\"") in
		let _ = output_string oc buf in
		let _ = output_string oc ("\nlet _ = close_out outch") in
	  let _ =  close_out oc in*)
		let _ = 
			if (Hashtbl.length pos_samples <> 0) then
				let newlines = List.fold_left (fun res solution -> 
					res @ (
						let env = ("\nlet _ = fprintf outch \"env:newtest\\t\\n\"") in
						let buf = ("\nlet _ = " ^ !Backwalker.main_function ^ " " ^ solution) in
						let newlines = [env; buf] in
						newlines
						)
					) [] solutions in
				let fi = ("\nlet _ = close_out outch") in	
				let newlines = 	newlines @ [fi] in
				let _ = Cbslearner.counter := 0 in
				modify instrumented_fname newlines in
		(* run the instrumented code for representative positive samples *)
		let _ = Hashtbl.clear pos_samples in				
		let _ = Run.sample instrumented_fname in 
		let _ = Instrument.read_dumpings pos_samples in
		(*let _ = Hashtbl.iter (fun loc (name, dumpings) -> 
			(fprintf std_formatter "At program location %s, a call to %s " loc name;
			List.iter (fun (locals, envs) -> 
				(fprintf std_formatter "are with parameters: ";
				(List.iter (fun local -> fprintf std_formatter "%s " local) locals);
				fprintf std_formatter "and with env variables: ";
				(List.iter (fun env -> fprintf std_formatter "%s " env) envs);
				fprintf std_formatter "@."
				)) dumpings
			)) pos_samples in*)
		pos_samples	
	else Hashtbl.create 0 (* Terminates if no new test can be generated *)		

(* Iteratively collecting more bad samples *)			
(** Possibly to retrieve a postive sample to guide the generation of a bad sample *)
let query_neg_samples env se_env unsounds prev_failed_invariants pos_samples = 
	let neg_samples = Hashtbl.create 13 in
	if !(Backwalker.hoflag) then
		(* New samples should be restricted by prev_failed_invariants *)
		let badbindings = 
			if (Hashtbl.length prev_failed_invariants = 0) then (
				let badbindings = Hashtbl.create (Hashtbl.length se_env.badbindings) in
				let _ = Hashtbl.iter (fun path bad -> 
					let allbindings = Frame.get_fun_bindings env (Hashtbl.find se_env.funframebindings path) in
					let bad = destruct_record bad in
					let bad = destruct_list_access bad in
					let bad = ease_bad_condition path allbindings bad pos_samples in
					Hashtbl.replace badbindings path 
					([Predicate.True], [Predicate.True], bad)
				) se_env.badbindings in
				badbindings
			) else (
				let badbindings = Hashtbl.create (Hashtbl.length se_env.badbindings) in
				(*let allpaths = Hashtbl.fold (fun k _ res -> res @ [k]) se_env.badbindings [] in*)
				let (*unresolvedpaths*) _ = Hashtbl.fold (fun path invs res ->
					(*let bad = Hashtbl.find se_env.badbindings path in*)
					let bad = 
						if !(se_env.dty) then {pre = Predicate.Not Predicate.True; post = Predicate.Not Predicate.True} 
						else (Hashtbl.find se_env.badbindings path) in
					let allbindings = Frame.get_fun_bindings env (Hashtbl.find se_env.funframebindings path) in
					let bad = destruct_record bad in
					let bad = destruct_list_access bad in
					let bad = ease_bad_condition path allbindings bad pos_samples in
					(*let unsounds = Hashtbl.find unsounds path in*)
					let unsounds = if !(se_env.dty) then [] else Hashtbl.find unsounds path in
					let funframe = Hashtbl.find se_env.funframebindings path in 
					let preinvs = Common.map_partial (fun (b,tbl,(_,inv)) -> 
						if (b) then (* postcondition driven *)
							Some (standarlize_invariant env funframe unsounds tbl inv bad.post)
						else None
					) invs.ipre in
					let postinvs = Common.map_partial (fun (b,tbl,(_,inv)) -> 
						if (b) then (* postconidtion driven *)
							Some (standarlize_invariant env funframe unsounds tbl inv bad.post)
						else None
					) invs.ipost in 
					let prerestriction = (*Predicate.big_or*) preinvs in
					let postrestriction = (*Predicate.big_or*) postinvs in
					(Hashtbl.replace badbindings path
					(prerestriction, postrestriction, bad(*{pre = Predicate.And (prerestriction, bad.pre);
					post = Predicate.And (postrestriction, bad.post)}*));
					res @ [path])
				) prev_failed_invariants [] in
				(*let _ = List.iter (fun up -> Format.fprintf Format.std_formatter "Function %s unresovled@." 
									(Path.name up)) unresolvedpaths in*)
				((*List.iter (fun ap -> 
					if (List.exists (fun up -> Path.same ap up) unresolvedpaths) then
						Hashtbl.replace bad_bindings ap (Predicate.Not Predicate.True)
					) allpaths*)
				badbindings)
			) in
		(* type of neg_samples: (Path.t, ((string * int) list) list) Hashtbl.t *)
		let _ = Format.fprintf Format.std_formatter "badbinding generated ...@." in
		let _ = Modelsolver.solve !(se_env.dty) env se_env.funframebindings badbindings unsounds neg_samples in
		(*let _ = Hashtbl.iter (fun path {Modelsolver.spre=prevalues; Modelsolver.spost=postvalues} -> 
			(fprintf std_formatter "A a call to %s with following negatives: \n" (Path.name path));
			ignore (List.fold_left (fun i value -> (
				fprintf std_formatter "For precondition index at %d: " i;
				List.iter (fun (name, value) -> 
					fprintf std_formatter "%s:%d " name value
					) value;
				fprintf std_formatter "@.";
				(i+1))) 0 prevalues);
			ignore (List.fold_left (fun i value -> (
				fprintf std_formatter "For postcondition index at %d: " i;
				List.iter (fun (name, value) -> 
					fprintf std_formatter "%s:%d " name value
					) value;
				fprintf std_formatter "@.";
				(i+1))) 0 postvalues);
			) neg_samples in
		let _ = Format.fprintf Format.std_formatter "bad sample of size %d collected ...@." 
			(Hashtbl.length neg_samples) in*)
		if ((*Hashtbl.length prev_failed_invariants = 0 &&
				Hashtbl.length neg_samples = 0 &&*) !(se_env.dty)) then 
			(* Inserting a fake sample in order to let invariant inference proceed *)
			(Hashtbl.iter (fun f _ -> 
				(*if String.compare (Path.name f) !Backwalker.main_function = 0 then*)
				if (Backwalker.is_measure se_env (Path.name f)) then
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[[("", 0)]]; 
					Modelsolver.spost=[]})
				else 
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[]; 
					Modelsolver.spost=[]})	
			) se_env.funframebindings; neg_samples)
		else 
			(Hashtbl.iter (fun f _ -> 
				if (Path.same f !Backwalker.main_function_path) then
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[]; 
					Modelsolver.spost=[]})	
			) se_env.funframebindings; neg_samples)
			(*neg_samples*)
	else
		if !(se_env.dty) then 
			(* Inserting a fake sample in order to let invariant inference proceed *)
			(Hashtbl.iter (fun f _ -> 
				(*if String.compare (Path.name f) !Backwalker.main_function = 0 then*)
				if (Backwalker.is_measure se_env (Path.name f)) then
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[[("", 0)]]; 
					Modelsolver.spost=[]})
				else if String.compare (Path.name f) !Backwalker.main_function = 0 then
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[[("", 0)]]; 
					Modelsolver.spost=[]})
				else 
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[]; 
					Modelsolver.spost=[]})	
			) se_env.funframebindings; neg_samples)
		else 
			(Hashtbl.iter (fun f _ -> 
				if String.compare (Path.name f) !Backwalker.main_function = 0 then
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[[("", 0)]]; 
					Modelsolver.spost=[]})
				else 
					(Hashtbl.replace neg_samples f {Modelsolver.spre=[]; 
					Modelsolver.spost=[]})	
			) se_env.funframebindings; neg_samples)
			

	
(* To avoid incorrect bad samples, filtering out function calls on the branch condition *)
let unsound_sources bad_c = 
	let rec loop p = match p with
		| P.Or (P.And (p0, p2), P.And (P.Not p1, p3)) ->
			if (p0 = p1) then
				(P.get_all_funs p1) @ (loop p2) @ (loop p3)
			else (*(loop p0) @ (loop p1) @ (loop p2) @ (loop p3)*) (loop p2) @ (loop p3)
		| P.Not p -> loop p
		| P.And (p1, p2) -> (loop p1) @ (loop p2)
		| P.Or (p1, p2) -> (loop p1) @ (loop p2)
		| _ -> [] in
	let potential_unsounds = Common.remove_duplicates (loop bad_c) in
	let allfuns = Common.remove_duplicates (P.get_all_funs bad_c) in
	let allfuns = Common.map_partial (fun fn ->
		if (List.exists (fun pu -> match (fn, pu) with
			| (P.FunApp _, P.FunApp _) -> fn = pu
			| _ -> assert false
		) potential_unsounds) then None else Some fn	
	) allfuns in
	Common.map_partial (fun pu ->
		if (List.exists (fun fn -> 
			match (pu, fn) with
				| (P.FunApp (p, _), P.FunApp (f, _)) ->
					(String.compare p f = 0) 
				| _ -> assert false	
		) allfuns) then Some pu
		else None
	) potential_unsounds		
		
let learn_from_samples learn_f env se_env unsounds cs atomics pos_samples neg_samples = 
	let invariants = Hashtbl.create 13 in
	let _ = learn_f atomics pos_samples neg_samples invariants se_env env in
	(*let _ = Hashtbl.iter (fun path {Cbslearner.ipre = preinvs; Cbslearner.ipost = postinvs} -> 
		(fprintf std_formatter "Function %s with precondition invariant %a@." (Path.name path)
		Predicate.pprint (Predicate.big_and (List.map (fun (_,_,(_,inv)) -> inv) preinvs));
		fprintf std_formatter "Function %s with postcondition invariant %a@." (Path.name path)
		Predicate.pprint (Predicate.big_and (List.map (fun (_,_,(_,inv)) -> inv) postinvs)))
		) invariants in*)
	let qualifiers = Hashtbl.fold (fun path {Cbslearner.ipre = preinvs; Cbslearner.ipost = postinvs} res -> 
		let funframe = Hashtbl.find se_env.funframebindings path in
		(*let _ = fprintf std_formatter "preinvs size = %d and postinvs size = %d@." (List.length preinvs) (List.length postinvs) in*)
		let bad = 
			if !(se_env.dty) then {pre = Predicate.Not Predicate.True; post = Predicate.Not Predicate.True} 
			else (Hashtbl.find se_env.badbindings path) in
		let bad = destruct_record bad in
		let bad = destruct_list_access bad in
		(*let suffixes = ["_0"; "_1"; "_2"; "_3"; "_4"; "_r"] in*)
		res @ (List.fold_left (fun res (b, tbl, (commons, inv)) -> 
			(*commons is only used when se_env.partial_used is set and commons does not contain _0 and _r*)
			let commons = if !(se_env.partial_used) 
				then (*List.filter (fun common -> List.for_all (fun var -> List.for_all (fun suf -> 
					not (Common.str_contains (Path.name var) suf)) suffixes) (Predicate.vars common))*) commons else [] in
			(*let _ = fprintf std_formatter "Found a pre-invariant: %a@." Predicate.pprint inv in*)
			(*let _ = List.iter (fun common -> Format.fprintf Format.std_formatter "common=%a@." Predicate.pprint common) commons in*)
			let funbadcondition = bad.pre in
			let unsounds = if !(se_env.dty) then [] else Hashtbl.find unsounds path in
			let commons = reject_commons env unsounds commons funbadcondition funframe tbl return_repr in
			(*let _ = List.iter (fun common -> Format.fprintf Format.std_formatter "common'=%a@." Predicate.pprint common) commons in*)
			let invs = split_invariant env funframe inv in
			List.fold_left (fun res inv -> 
				let path = Path.Pident (Ident.create_with_stamp ((Path.name path) ^ "Pre") (Path.stamp path)) in
				let (k, (p,valu,pred)) = from_invariant b env (se_env.measures) inv path funframe tbl return_repr commons in
				(*let _ = fprintf std_formatter "Translate the found invariant into %a@." Predicate.pprint pred in*)
				res @ [(k,(Path.mk_ident (Path.name p),valu,pred))]	
			) res invs
			) [] preinvs) @
			(List.fold_left (fun res (b, tbl, (commons, inv)) -> 
			let commons = if !(se_env.partial_used) 
				then (*List.filter (fun common -> List.for_all (fun var -> List.for_all (fun suf -> 
					not (Common.str_contains (Path.name var) suf)) suffixes) (Predicate.vars common))*) commons else [] in 
			(*let _ = Format.fprintf Format.std_formatter "Found a post-invariant: %a@." Predicate.pprint inv in*)
			(*let _ = List.iter (fun common -> Format.fprintf Format.std_formatter "common=%a@." Predicate.pprint common) commons in*)
			let funbadcondition = bad.post in
			let unsounds = if !(se_env.dty) then [] else Hashtbl.find unsounds path in
			let commons = reject_commons env unsounds commons funbadcondition funframe tbl return_repr in
			(*let _ = List.iter (fun common -> Format.fprintf Format.std_formatter "common'=%a@." Predicate.pprint common) commons in*)
			let invs = split_invariant env funframe inv in 
			List.fold_left (fun res inv -> 
				let suffix = 
					if !(Clflags.reachability) && Predicate.is_shape_pred inv then 
						if (List.mem F.returnpath (Predicate.vars inv)) then "Post" else "Pre"
					else "Post" in
				let path = Path.Pident (Ident.create_with_stamp ((Path.name path) ^ suffix) (Path.stamp path)) in
				let (k, (p,valu,pred)) = from_invariant b env (se_env.measures) inv path funframe tbl return_repr commons in
				(*let _ = fprintf std_formatter "Translate the found invairant into %a@." Predicate.pprint pred in*)
				res @ [(k,(Path.mk_ident (Path.name p),valu,pred))]
			) res invs
			) [] postinvs) 
		) invariants [] in
	(*let _ = Format.fprintf Format.std_formatter "Number of invariants %d@." (List.length qualifiers) in
	
	let _ = List.iter (fun (_,(_,_,p)) -> Format.fprintf Format.std_formatter "q = %a@." Predicate.pprint p) qualifiers in*)
	
	(*let forbid_ks = Hashtbl.fold (fun _ f res -> 
		res @ (Common.map_partial (fun x->x) (F.get_refinements f))) (se_env.funframebindings) [] in*)
		
	(** Optimization for learning data structure invariants *)			
	let qualifiers = 
		if !(Clflags.no_hoflag) then 
			let qualifiers = (List.map (fun (k, q) -> q) qualifiers) in
			List.flatten (List.map (fun q -> 
				List.map (fun q -> (None, q)) (Bstats.time "instantiating quals" (instantiate_in_environments cs) [q])
				) qualifiers)
		else
		List.flatten (List.map (fun (k,((path,_,_) as q)) -> 
			(*let forbid_ks = match k with
				| Some k -> 
					List.fold_left (fun res k' -> 
					if (Path.same k' k) then res else res @ [k'] 
					) [] forbid_ks 
				| None -> forbid_ks in*)
			List.map (fun q -> (k, q)) (Bstats.time "instantiating quals" (instantiate_in_environments cs) [q])
			) qualifiers) in
	(** Furthur translate the instantiated invaraint over old(a) to old_a *)		
	let qualifiers = 
		List.map (fun (k, (a,b,p)) -> 
			(k, (a, b, Predicate.map_expr (fun expr -> match expr with
				| Predicate.FunApp (fn, args) when (String.compare fn Frame.old_array_fun = 0)-> 
					(assert (List.length args = 1);
					let a = Predicate.exp_var (List.hd args) in
					let stamp = Path.stamp a in
					(Predicate.Var (Path.Pident (Ident.create_with_stamp (Frame.old_array_flag^(Path.name a)) stamp))))
				| expr -> expr
			) p))	
		) qualifiers in
	let _ = Format.fprintf Format.std_formatter "Number of qualifiers after instantiation %d@." (List.length qualifiers) in
	(*let _ = List.iter (fun (_,(_,_,p)) -> Format.fprintf Format.std_formatter "q = %a@." Predicate.pprint p) qualifiers in*)
	(*let _ = if (List.length qualifiers = 0) then assert false in*)
	(invariants, qualifiers)

let prepare_unsounds se_env = 
	let unsounds = Hashtbl.create (Hashtbl.length (se_env.badbindings)) in
	let _ = Hashtbl.iter (fun path badc ->
		Hashtbl.replace unsounds path (
		let fs = unsound_sources badc.post in
		Common.map_partial (fun f -> match f with
			| P.FunApp (fn, es) -> 
				if (String.compare fn "UF" = 0) then
					let (hd, tl) = (List.hd es, List.tl es) in
					match hd with
						| P.Var funvar -> Some (P.FunApp (Path.name funvar, tl))
						| _ -> (** Fixme? *) assert false
				else None
			| _ -> assert false) fs	
		)
	) se_env.badbindings in
	unsounds
	
(* Generate atomic predicates from source code *)	
let query_atomics env se_env () = 
	Hashtbl.fold (fun path bad res -> 
		(*res @ (List.map (fun p -> (path, Path.mk_ident "", p)) (Backwalker.find_atomics path))*)
		if !(Backwalker.data_structure_dealing_flag) then res
		else
		let allbindings = Frame.get_fun_bindings env (Hashtbl.find se_env.funframebindings path) in
		let bad = destruct_record bad in
		let bad = destruct_list_access bad in
		let bad = ease_bad_condition path allbindings bad pos_samples in
		let qs = ref [] in
		let _ = (Predicate.map_pred (fun pred -> match pred with
			| Predicate.Atom _ when (Predicate.vars pred <> [] ) -> 
				(qs := (path, Path.mk_ident "", pred)::(!qs); pred)
			| pred -> pred) bad.post) in
		(*let _ = (Predicate.map_pred (fun pred -> match pred with
			| Predicate.Atom _ when (Predicate.vars pred <> []) -> 
				(qs := (path, Path.mk_ident "", pred)::(!qs); pred)
			| pred -> pred) bad.pre) in*)
		let qs = Common.remove_duplicates (!qs) in 
		res @ qs 
	) se_env.badbindings []
	
(** Find post-conditions that requires more than 1 coeffs *)
let find_assertqs post =
	let preds = Predicate.fullsplit post in
	let preds = List.filter (fun pred -> 
		let vars = Predicate.vars pred in
		if (List.exists (fun var -> Path.same var F.returnpath) vars) then
			let coeffs = Predicate.coeffs pred in
			List.exists (fun c -> c > 1 || c < -1) coeffs
		else false
		) preds in
	List.map (fun pred -> 
		(Path.mk_ident "", valu, 
				Predicate.subst pvalu F.returnpath pred)) preds		
 
let qualify_implementation sourcefile fenv ifenv env qs str tystr =
	(** do collect built-in functions *)
	let builtin_funs = Lightenv.domain fenv in
	(** do collect information for backward symbolic execution *)
	let se_env = {funbindings = Hashtbl.create 13; funframebindings = Hashtbl.create 13;
								hobindings = Hashtbl.create 13; badbindings = Hashtbl.create 13;
								effectbindings = Hashtbl.create 13;
								returnbindings = Hashtbl.create 13;
								assertbindings = Hashtbl.create 13;
								frames = (fun exp -> FrameExpLog.find exp (!fexplog)); 
								builtin_funs = builtin_funs; funcalls = ref [];
								funcallenvs = Hashtbl.create 13;
								fundefenvs = Hashtbl.create 13;
								partial_used = ref false;
								dty = ref false;
								measures = Hashtbl.create 3;
								measure_defs = Hashtbl.create 3
								} in
  let (qs, fenv, cs) = constrain_structure se_env fenv qs tystr in
	
	(** do get all the functions *)	
	let (hoflag, functions) = Hashtbl.fold (fun p fr (flag, res) -> 
		if flag then (flag, p::res)
		else 
			let frs = Frame.get_fun_bindings env fr in
			let flag = List.exists (fun (_, fr) -> 
				match fr with 
					| Frame.Farrow _ -> true 
					| Frame.Fconstr (p,_,_,_,_) when 
						(Path.same Predef.path_list p && Hashtbl.length se_env.measures = 0) 
						-> true
					| _ -> false) frs in
			(flag, p::res)) se_env.funframebindings (false, []) in	
	(* Higher order program and list-manipulating programs are verifed by *)
	(* both postive samples and negative samples *)		
	let hoflag = 
		(* If user want to check higher-order function, check it *)
		if (!Clflags.hoflag) then true 
		(* If user do not wish to check higher-order function, do not check it *)
		else if (!Clflags.no_hoflag) then false
		(* Let us decide whether we should check higher-order function *)
		else hoflag in 
	let _ = Backwalker.hoflag := hoflag in
	
	(** do a symbolic execution to collect symbolic constraints *)
	let _ = Backwalker.symb_exe_structure se_env tystr in 
	(*let _ = Hashtbl.iter (fun p {pre = prebad; post = postbad} -> 
		(fprintf std_formatter "function %s with pre badbinding %a @." 
		(Path.name p) Predicate.pprint prebad;
		fprintf std_formatter "function %s with post badbinding %a @." 
		(Path.name p) Predicate.pprint postbad)
		) se_env.badbindings in 
	let _ = Hashtbl.iter (fun p effcons -> 
		 fprintf std_formatter "function %s with underapproximate effect %a @." 
		(Path.name p) Predicate.pprint effcons
		) se_env.effectbindings in		
	let _ = Hashtbl.iter (fun p preturn -> 
		 fprintf std_formatter "function %s with return pred %a @." 
		(Path.name p) Predicate.pprint preturn
		) se_env.returnbindings in	
	let _ = Hashtbl.iter (fun p assertion ->
		 fprintf std_formatter "function %s with assertion %a @."
		(Path.name p) Predicate.pprint assertion
		) se_env.assertbindings in	*)
		
	(** do extract qualifiers from user-assertions when hoflag is set *)
	let qs = 
		if hoflag then
			let assert_qs = Hashtbl.fold (fun p {pre = prebad; post= postbad} res -> 
				res @ (find_assertqs postbad)
			) se_env.badbindings [] in
			qs @ assert_qs 
		else qs in		
			
	(** do let the wellformed solver know the measures for data types *)	
	let _ = (Wellformed.measures := se_env.measures) in
	let _ = (Wellformed.udt_table := udt_table) in
			
	(** do let the intrumentor know the measures for data types  *)
	let datatypes = Hashtbl.fold (fun p _ res -> p::res) se_env.measures [] in
	let _ = Hashtbl.iter (fun f fr -> match fr with
		| Frame.Farrow (_, Frame.Fconstr (p1,_,_,_,_), Frame.Fconstr (p2,_,_,_,_), _) 
		when (List.exists (fun dty -> Path.same p1 dty) datatypes && Path.same p2 Predef.path_int)-> 
			(* This is a measure for data type p1*)
			(* We associate the recflag with the measures *)
			let (recflag, _) = Hashtbl.find se_env.funbindings f in
			Hashtbl.replace se_env.measures p1 ((f, recflag)::(Hashtbl.find se_env.measures p1))
		| _ -> ()
	) se_env.funframebindings in
	
	(** do formally specify the measures *)
	let _ = Datatype.symbexe_measure se_env env in
	let cs = if !(se_env.dty) then Datatype.update_measure se_env udt_table cs else cs in
	
	(** do instrument source code *)
	let fname = Misc.chop_extension_if_any sourcefile in
	let printer = new Pprintast.printer fname env fenv 
												(fun loc -> FrameLog.find loc (!flog)) se_env udt_table in
	let modified_source = 
		ignore (flush_str_formatter ());
  	printer#structure str_formatter str;
  	flush_str_formatter () in
	(*let _ = print_string modified_source in*)
	let instrumented_fname = fname ^ "_instru.ml" in
	let intru_ch = open_out instrumented_fname in 
	let _ = Printf.fprintf intru_ch "%s" modified_source in 
	let _ = close_out intru_ch in
	
	(** do concrete runs of program to collect + samples *)
	(** do SMT solving to collect - samples *)
	(** do the learning *)
	
	(** do solving with the inferred qualifiers *)
	let cs = (List.map (lbl_dummy_cstr env) (Le.maplistfilter (mfm fenv) ifenv)) @ cs in
	(* do the invariant solving *)
	let _ = pre_solve () in
  let inst_qs = Bstats.time "instantiating quals" (instantiate_in_environments cs) qs in
	(*let inst_qualifiers = Bstats.time "instantiating quals" (instantiate_in_environments cs) qualifiers in*)
	(*let _ = List.iter (fun (path, valu, pred) ->
		fprintf std_formatter "Name %s with qualifier %a @." (Path.name path)
		Predicate.pprint pred
		) inst_qs in*)
	(* In order to do solving, must present solver which function is higher order *)
	let is_higher_order var fr = 
		((*fprintf std_formatter "var %s will be visited@." (Path.name var);*)
		let result =  match fr with
		| Frame.Farrow _ ->
			not (Hashtbl.mem se_env.funbindings var || 
					List.exists (fun p -> Path.same p var) se_env.builtin_funs) 
		| _ -> false in
		((*fprintf std_formatter "var %s is decided as %b@." (Path.name var) result;*) result)) in
	let unsounds = prepare_unsounds se_env in
	
	(* Copy a se_env for iteratively driving tests *)
	let se_env' = {funbindings = Hashtbl.copy se_env.funbindings; 
								funframebindings = Hashtbl.copy se_env.funframebindings;
								hobindings = Hashtbl.copy se_env.hobindings; 
								badbindings = Hashtbl.copy se_env.badbindings;
								effectbindings = Hashtbl.copy se_env.effectbindings;
								returnbindings = Hashtbl.copy se_env.returnbindings;
								assertbindings = Hashtbl.copy se_env.assertbindings;
								frames = (fun exp -> FrameExpLog.find exp (!fexplog)); 
								builtin_funs = builtin_funs; funcalls = ref (!(se_env.funcalls));
								funcallenvs = Hashtbl.copy se_env.funcallenvs;
								fundefenvs = Hashtbl.copy se_env.fundefenvs;
								partial_used = ref !(se_env.partial_used);
								dty = ref !(se_env.dty);
								measures = Hashtbl.copy se_env.measures;
								measure_defs = Hashtbl.copy se_env.measure_defs
								} in
								
	(* Analyaze how constants would affect the precison of our implementation *)
	let templates = Backwalker.gen_atomics () in			
	let algnumber = 
		(* Higher-order programs and list-manipulating programs are verified by*)
		(* both postive samples and negative samples *)
		if hoflag then 2 else 3 in
	(* Go into iterative learning *)
  let (s,cs,iter_count) = Bstats.time "solving" (solve functions
		(** A higher order function will be constrained in typing formulas *)
		is_higher_order 
		(** A query for atomic predicates *)
		(query_atomics env se_env)
		(** A query for postive_samples *)
		(query_pos_samples env se_env' unsounds tystr instrumented_fname)
		(*query_pos_samples instrumented_fname*)
		(** A query for negative_samples *)
		(query_neg_samples env se_env unsounds)
		(** A learner for invariants *)
		(learn_from_samples (Cbslearner.learn algnumber templates udt_table) env se_env unsounds cs)
		(** A learner for array invariants *)
		(learn_from_samples Cbslearner.gen_inv env se_env unsounds cs)
		(** User specified atomic predicates *)
		inst_qs
		(*inst_qualifiers*)) cs in
  let _ = post_solve () in
	
  let channel, channelname = 
		if (!Cf.dump_specs) then
			let filename = "./specifications.txt" in
			(formatter_of_out_channel (open_out filename), filename)
		else 
			let _ = Format.fprintf Format.std_formatter "\nShow specification synthesis result: @." in
			(std_formatter, "the above command lines") in

	(*let _ = Format.fprintf Format.std_formatter "##Number of iteration: %d##@." iter_count in*)
	(*let _ = Format.fprintf Format.std_formatter "Number of tests: %d@." !(Backwalker.tests) in*)
	let nb_invs = Hashtbl.fold (fun p f res -> 
		let _ = fprintf channel "function %s with type %a @." 
			(Path.name p) Frame.pprint (Frame.apply_solution s f) in
		let ks = Frame.all_refinement_vars f in
		List.fold_left (fun res k -> 
			List.fold_left (fun res (n,_,_) -> 
				let n = Path.name n in
					if (Common.str_contains n (Path.name p)) 
					then res + 1 else res 				
				) res (s k)
			) res ks
		) se_env.funframebindings 0 in
	
	let _ = Format.fprintf Format.std_formatter "@." in		
	let _ = Format.fprintf Format.std_formatter "##Size of hypothesis domain: %d##@." !(Cbslearner.nb_hypo) in	
	let _ = Format.fprintf Format.std_formatter "##In total %d specifications were synthesized in %s. QED.@." nb_invs channelname in
	(*let _ = Format.fprintf Format.std_formatter "Number of verified disjunctive invariants : %d@." (List.length d_invs) in*)
  (*let _ = dump_frames sourcefile (framemap_apply_solution s !flog) in*)
  match cs with [] -> () | _ -> 
    (Printf.printf "Errors encountered during type checking:\n\n";
    flush stdout; raise (Errors(List.map (make_frame_error s) cs)))
