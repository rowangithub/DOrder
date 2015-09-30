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

open Format
open Wellformed
open Modelsolver
module F = Frame
module Le = Lightenv
module Pat = Pattern
module P = Predicate
module TP = TheoremProver
module B = Builtins

module C = Common
module VM = C.PathMap 
module Sol = Hashtbl.Make(C.ComparablePath)
module Cf = Clflags

(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type fc_id = int option 
type subref_id = int 

module SIM = Map.Make(struct type t = subref_id let compare = compare end)

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of F.t Le.t * guard_t * F.t * F.t
  | WFFrame of F.t Le.t * F.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t 
  | Assert of Location.t 
  | Cstr of labeled_constraint

type refinement_constraint =
  | SubRef of F.t Le.t * guard_t * F.refinement * F.refinement * (subref_id option) 
  | WFRef of F.t Le.t * F.refinement * (subref_id option) 

(* Our tool infers functional type only -- we are interested in functional unknowns only *)
let functional_unknows = ref [] 

(* Also Fixme. No need for effect for the following 3 function? *)
let f_params_return_refinements cs = (* search for unknown types for function parameters and return *)
	let res = (List.fold_left (fun (res1, res2) c -> 
		(match c.lc_orig with
			| Cstr _ -> assert false
			| o ->
				let cstr = c.lc_cstr in
				match cstr with
					| SubFrame (_, _, f1, f2) -> (match (f1, f2) with
						| (Frame.Farrow _, Frame.Farrow _) -> 
							(res1 @ (Frame.all_refinement_vars f1) @ (Frame.all_refinement_vars f2),
							(Frame.get_refinement_variable f1)::((Frame.get_refinement_variable f2)::res2))
						| (f, f') -> (res1, res2)
						)
					| WFFrame (_, f) -> (match f with 
						| Frame.Farrow _ -> (res1 @ (Frame.all_refinement_vars f), (Frame.get_refinement_variable f)::res2)
						| f -> (res1, res2)))	
	) ([], []) cs) in
	(Common.remove_duplicates (fst res), 
	Common.remove_duplicates (Common.map_partial (fun x -> x) (snd res)))

let ho_param_refinements cs = (*Fixme in ho_param_refinements_from_frame for Frecord etc*)
	let rec ho_param_refinements_from_frame fr = match fr with
		| Frame.Farrow (None,f1,f2,_) -> 
			(Common.map_partial (fun x->x) (Frame.get_refinements f1)) @
			(ho_param_refinements_from_frame f2)
		| Frame.Farrow (Some _,f1,f2,_) ->
			(*ho_param_refinements_from_frame f2*)
			(ho_param_refinements_from_frame f1) @ (ho_param_refinements_from_frame f2)
		| _ -> [] in
	Common.remove_duplicates (List.fold_left (fun res c -> 
	res @ (match c.lc_orig with
	| Cstr _ -> assert false
	| _ ->
		let cstr = c.lc_cstr in 
		match cstr with
			| SubFrame (_, _, f1, f2) -> 
				(ho_param_refinements_from_frame f1) @
				(ho_param_refinements_from_frame f2)
			| WFFrame (_, f) -> (ho_param_refinements_from_frame f))
	) [] cs)
	
let ho_return_refinements cs = 
	let rec ho_return_refinements_from_frame fr = match fr with
		| Frame.Farrow (None,f1,f2,_) -> 
			Common.map_partial (fun x->x) (
			[Frame.get_refinement_variable f2])
		| Frame.Farrow (Some _,f1,f2,_) ->
			(*Common.map_partial (fun x->x) (
			[Frame.get_refinement_variable f2])*)
			(ho_return_refinements_from_frame f1) @ (ho_return_refinements_from_frame f2)
		| _ -> [] in
	Common.remove_duplicates (List.fold_left (fun res c -> 
	res @ (match c.lc_orig with
	| Cstr _ -> assert false
	| _ ->
		let cstr = c.lc_cstr in 
		match cstr with
			| SubFrame (_, _, f1, f2) -> 
				(ho_return_refinements_from_frame f1) @
				(ho_return_refinements_from_frame f2)
			| WFFrame (_, f) -> (ho_return_refinements_from_frame f))
	) [] cs)
	
let array_refinements cs = 
	let rec array_refinements_from_frame fr = match fr with 
		| Frame.Fconstr (x,fl,_,_,effect) ->
			let arr_refinements = 
				List.fold_left (fun res f -> res@(array_refinements_from_frame f)) [] fl in
			let effect_refinements = 
				List.fold_left (fun res (_, f) -> res@(array_refinements_from_frame f)) [] effect in
			if (x = Predef.path_array) then arr_refinements @ effect_refinements @
				(Common.map_partial (fun x->x) [Frame.get_refinement_variable fr])
			else arr_refinements @ effect_refinements
		| Frame.Farrow (_, f1, f2, _) ->
			(array_refinements_from_frame f1) @ (array_refinements_from_frame f2)
		| Frame.Ftuple (fl, _) -> 
			List.fold_left (fun res f -> res@(array_refinements_from_frame f)) [] fl
		| Frame.Frecord (_,fl,_) ->
			List.fold_left (fun res (f,_,_) -> res@(array_refinements_from_frame f)) [] fl
		| _ -> [] in
	Common.remove_duplicates (List.fold_left (fun res c -> 
	res @ (match c.lc_orig with
	| Cstr _ -> assert false
	| _ ->
		let cstr = c.lc_cstr in 
		match cstr with
			| SubFrame (_, _, f1, f2) -> 
				(array_refinements_from_frame f1) @
				(array_refinements_from_frame f2)
			| WFFrame (_, f) -> (array_refinements_from_frame f))
	) [] cs)		

let ex_counter = ref 0		
let newEX () = (
	let i = !ex_counter in 
	(ex_counter := (i+1);
	Path.mk_ident ("EX_"^(string_of_int i))))
		

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

(* Unique variable to qualify when testing sat, applicability of qualifiers...
 * this is passed into the solver *)
let qual_test_var = Path.mk_ident "AA"
let qual_test_expr = P.Var qual_test_var


(* Unique variables to qualify when testing for-all sat, *)
(* this is passed into the solver *)
let forall_uvar = Path.mk_ident "u"
let forall_uexpr = P.Var forall_uvar
let forall_vvar = Path.mk_ident "v"
let forall_vexpr = P.Var forall_vvar

let is_simple_constraint = function 
  SubRef (_, _, ([],F.Qvar _), ([], F.Qvar _), _) -> true | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let solution_map s k = 
  C.do_catch 
    ((*Printf.sprintf "ERROR: solution_map couldn't find: %s" (Path.name k)*)"")
    (Sol.find s) k

(**************************************************************)
(**************************** Stats ***************************)
(**************************************************************)
let stat_wf_refines = ref 0
let stat_sub_refines = ref 0
let stat_simple_refines = ref 0
let stat_refines = ref 0
let stat_imp_queries = ref 0
let stat_valid_imp_queries = ref 0
let stat_matches = ref 0 

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let guard_predicate () g = 
  P.big_and 
    (List.map 
      (fun (v,b) -> 
         let p = P.equals (B.tag (P.Var v), P.PInt 1) in
         if b then p else P.Not p) 
      g)

let environment_predicate is_higher_order cache sm env =
	P.big_and (
		Common.map_partial (fun x -> x) (Le.maplist (fun v f -> 
		match f with
		| Frame.Farrow _ when not (Hashtbl.mem cache v) -> 
			if (is_higher_order v f) then (
				let pred = (F.predicate cache sm (P.Var v) f) in
				(Hashtbl.replace cache v (); Some pred))
			else None
		| f when not (Hashtbl.mem cache v) -> 
			(Hashtbl.replace cache v (); Some (F.predicate cache sm (P.Var v) f))
		| f -> None) env)
	)

let pprint_local_binding ppf = function
  | (Path.Pident _ as k, v) -> fprintf ppf "@[%s@;=>@;<1 2>%a@],@;<1 2>" (Path.unique_name k) F.pprint v
  | _ -> ()

(* To make it compact, do not dumpt higher order encoding in printing methods *)
let pprint_env_pred so ppf env =
  match so with
  | Some s -> P.pprint ppf (environment_predicate (fun _ _ -> false) (Hashtbl.create 0) (solution_map s) env)
  | _ -> Le.iter (fun x t -> pprint_local_binding ppf (x, t)) env

let pprint ppf = function
  | SubFrame (_,_,f1,f2) ->
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (_,f) ->
      F.pprint ppf f

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = function
  | SubRef (env,g,r1,r2,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@;<1 0>|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_env_pred so) env P.pprint (guard_predicate () g) 
      F.pprint_refinement r1 F.pprint_refinement r2 
  | WFRef (env,r,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_env_pred so) env F.pprint_refinement r 


(**************************************************************)
(************* Constraint Simplification & Splitting **********) 
(**************************************************************)

let simplify_frame gm x f = 
  if not (Le.mem x gm) then f else
    let pos = Le.find x gm in
    match f with 
    | F.Fconstr (a,b,c,(subs,F.Qconst[(v1,v2,P.Iff (v3,p))]),effect) when v3 = B.tag (P.Var v2) ->
        let p' = if pos then p else P.Not p in
        F.Fconstr (a,b,c,(subs,F.Qconst[(v1,v2,p')]),effect) 
    | F.Frecord (a,b,(subs,F.Qconst[(v1,v2,P.Iff (v3,p))])) when v3 = B.tag (P.Var v2) ->
        let p' = if pos then p else P.Not p in
        F.Frecord (a,b,(subs,F.Qconst[(v1,v2,p')]))
    | F.Ftuple (fs,(subs,F.Qconst[(v1,v2,P.Iff (v3,p))])) when v3 = B.tag (P.Var v2) ->
        let p' = if pos then p else P.Not p in
        F.Ftuple (fs,(subs,F.Qconst[(v1,v2,p')]))
    | _ -> f

let simplify_env env g =
  let gm = List.fold_left (fun m (x,b)  -> Le.add x b m) Le.empty g in
  Le.fold 
    (fun x f env' ->
      match f with | F.Fconstr _ | F.Frecord _ | F.Ftuple _ | F.Farrow _ ->
        Le.add x (simplify_frame gm x f) env' 
      | _ -> env')
    env Le.empty

let simplify_fc c =
  match c.lc_cstr with
  | WFFrame _ -> c 
  | SubFrame (env,g,a,b) ->
      let env' = simplify_env env g in
      (* let _ = printf "@[Simplify env to: %a@.@]" (pprint_env_pred None) env' in *)
      {c with lc_cstr = SubFrame(env', g, a, b)}

(* Notes:
  * 1. If the labels disagree, we don't resolve. Instead, we 
  * proceed with the best information we have, probably losing 
  * chances to assert qualifiers along the way. 
  * 2. pmr: because we were only filtering through invariant types 
  * anyway, we might as well just use invariants until we start 
  * getting problems from it --- for now, it's too much trouble 
  * to work around all the BigArray stuff
  *)

let make_lc c fc ho_index = ({lc_cstr = fc; lc_tenv = c.lc_tenv; lc_orig = Cstr c; lc_id = c.lc_id}, ho_index)

let lequate_cs env g c ho_index variance f1 f2 = match variance with
  | F.Invariant -> [make_lc c (SubFrame(env,g,f1,f2)) ho_index; make_lc c (SubFrame(env,g,f2,f1)) ho_index]
  | F.Covariant -> [make_lc c (SubFrame(env,g,f1,f2)) ho_index]
  | F.Contravariant -> [make_lc c (SubFrame(env,g,f2,f1)) ho_index]

let match_and_extend tenv env ho_index (l1,f1) (l2,f2) =
  match (l1,l2) with
  | (Some p, None) | (None, Some p) -> 
		(* Do an encoding so named function and higher order function can be subtyped *)
		let env' = 
			Le.add (Path.mk_ident "ho_pattern")
        (B.mk_int [(Path.mk_ident "", Path.mk_ident "", 
					Pattern.desugar_bind p (Predicate.Var (Frame.get_ho_param ho_index)))])
        env in
		F.env_bind tenv env' p f2
  | (Some p1, Some p2) when Pat.same p1 p2 -> F.env_bind tenv env p1 f2
  | _  -> env (* 1 *)
 
let mutable_variance = function Asttypes.Mutable -> F.Invariant | _ -> F.Covariant


(** Effective Subtyping. Is ho_index useful for this analysis? *)

(** Effective Subtyping. Is ho_index useful for this analysis? *)
(*let effect_sub env g c ho_index effect effect' = 
	List.fold_left (fun res (p', fr') ->
		if (List.mem_assoc p' effect) then	
			let (_, fr) = List.find (fun (p, fr) -> Path.same p' p) effect in
			let effect = List.remove_assoc p' effect in
			(* env should be extened with effect and with the binding of p removed *)
			(*let env = if (Lightenv.mem p' env) then Lightenv.remove p' env else env in*)
			let env = Frame.update_env env effect (*Le.addn effect env*) in
			res @ (lequate_cs env g c 0 F.Covariant fr fr')
		else 
			(let env = Frame.update_env env effect (*Lightenv.addn effect env*) in
			try 
				let (fr, env) = (Le.find p' env, Le.remove p' env) in
				res @ (lequate_cs env g c 0 F.Covariant fr fr')
			with _ -> assert false)
	) [] effect'

let split_sub = function ({lc_cstr = WFFrame _},_) -> assert false 
	| ({lc_cstr = SubFrame (env,g,f1,f2); lc_tenv = tenv} as c, ho_index) ->
  match (f1, f2) with
  | (F.Farrow (l1, f1, f1', effect), F.Farrow (l2, f2, f2', effect')) -> 
      let env' = match_and_extend tenv env ho_index (l1,f1) (l2,f2) in
      ((lequate_cs env g c ho_index F.Covariant f2 f1) @ (lequate_cs env' g c (ho_index+1) F.Covariant f1' f2')
				@ (effect_sub env g c ho_index effect effect'),
       [])
  | (F.Fvar _, F.Fvar _) | (F.Funknown, F.Funknown) ->
      ([],[]) 
  | (F.Fconstr (p1, f1s, variances, r1, effect), F.Fconstr(p2, f2s, _, r2, effect')) ->  (* 2 *)
      ((C.flap3 (lequate_cs env g c ho_index) variances f1s f2s) @ 
				(effect_sub env g c ho_index effect effect'),
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Ftuple (f1s, r1), F.Ftuple (f2s, r2)) ->
      (C.flap2 (lequate_cs env g c ho_index F.Covariant) f1s f2s,
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Frecord (_, fld1s, r1), F.Frecord (_, fld2s, r2)) ->
      (C.flap2 
         (fun (f1',_,m) (f2',_,_) -> lequate_cs env g c ho_index (mutable_variance m) f1' f2')
         fld1s fld2s, 
       if List.exists (fun (_, _,m) -> m = Asttypes.Mutable) fld1s
       then [(Cstr c, SubRef (env,g,r1,r2,None)); (Cstr c, SubRef (env,g,r2,r1,None))]
       else [(Cstr c, SubRef (env,g,r1,r2,None))])
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf = function ({lc_cstr = SubFrame _},_) -> assert false 
	| ({lc_cstr = WFFrame (env,f); lc_tenv = tenv} as c, ho_index) ->
  let make_wff env ho_index f = ({lc_cstr = WFFrame (env, f); lc_tenv = tenv; lc_orig = Cstr c; lc_id = None}, ho_index) in
  match f with
  | F.Fconstr (_, l, _, r, effect) ->
      ((List.map (make_wff env ho_index) l) @ (List.map (fun (p, fr) -> 
				let env = if (Lightenv.mem p env) then (Lightenv.remove p env) else env in
				make_wff env ho_index fr) effect),
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Farrow (l, f, f', effect) ->
      let env' = match l with 
				| None -> 
					(Le.add (Frame.get_ho_param ho_index) f env)
				| Some p -> F.env_bind tenv env p f in
      ([make_wff env ho_index f; make_wff env' (ho_index+1) f'] @ 
			(List.map (fun (p, fr) -> 
				let env = if (Lightenv.mem p env) then (Lightenv.remove p env) else env in
				make_wff env ho_index fr) effect), [])
  | F.Ftuple (fs, r) ->
      (List.map (make_wff env ho_index) fs, [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Frecord (_, fs, r) ->
      (List.map (fun (f',_,_) -> make_wff env ho_index f') fs,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Fvar _ | F.Funknown ->
      ([],[])*)


let effect_sub env g c ho_index effect effect' = 
	let env = Frame.update_env env effect in
	List.fold_left (fun res (p', fr') ->
		try 
			let (fr(*, env*)) = (Le.find p' env(*, Le.remove p' env*)) in
			res @ (lequate_cs env g c 0 F.Covariant fr fr')
		with _ -> assert false	
	)	[] effect'

(* check if all domain in effect1 is bounded in effect2 *)
let dom_same effect1 effect2 = 
	(List.length effect1 = List.length effect2) && 
	(List.for_all (fun (p1, _) -> List.mem_assoc p1 effect2) effect1)

let split_sub = function ({lc_cstr = WFFrame _},_) -> assert false 
	| ({lc_cstr = SubFrame (env,g,f1,f2); lc_tenv = tenv} as c, ho_index) ->
  match (f1, f2) with
  | (F.Farrow (l1, f1, f1', effect), F.Farrow (l2, f2, f2', effect')) -> 
      let env' = match_and_extend tenv env ho_index (l1,f1) (l2,f2) in
      ((lequate_cs env g c ho_index F.Covariant f2 f1) @ (lequate_cs env' g c (ho_index+1) F.Covariant f1' f2')
				@ (effect_sub env g c ho_index effect effect'),
       [])
  | (F.Fvar _, F.Fvar _) | (F.Funknown, F.Funknown) ->
      ([],[]) 
  | (F.Fconstr (p1, f1s, variances, r1, effect), F.Fconstr(p2, f2s, _, r2, effect')) ->  (* 2 *)
			let variances = 
				if (Path.same p1 Predef.path_array) (* Fixme. I am not sure about this soundness ... *)
				then (List.map (fun _ -> F.Covariant) variances) else variances in
      ((C.flap3 (lequate_cs env g c ho_index) variances f1s f2s) @ 
				((* We allow the effect to refer to return variable *)
				let env = 
					if (dom_same effect effect') then env
					else Lightenv.addn [(Frame.returnpath, (F.Fconstr (p1, f1s, variances, r1, [])))] env in
				effect_sub env g c ho_index effect effect'),
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Ftuple (f1s, r1), F.Ftuple (f2s, r2)) ->
      (C.flap2 (lequate_cs env g c ho_index F.Covariant) f1s f2s,
       [(Cstr c, SubRef(env,g,r1,r2,None))])
  | (F.Frecord (_, fld1s, r1), F.Frecord (_, fld2s, r2)) ->
      (C.flap2 
         (fun (f1',_,m) (f2',_,_) -> lequate_cs env g c ho_index (mutable_variance m) f1' f2')
         fld1s fld2s, 
       if List.exists (fun (_, _,m) -> m = Asttypes.Mutable) fld1s
       then [(Cstr c, SubRef (env,g,r1,r2,None)); (Cstr c, SubRef (env,g,r2,r1,None))]
       else [(Cstr c, SubRef (env,g,r1,r2,None))])
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf = function ({lc_cstr = SubFrame _},_) -> assert false 
	| ({lc_cstr = WFFrame (env,f); lc_tenv = tenv} as c, ho_index) ->
  let make_wff env ho_index f = ({lc_cstr = WFFrame (env, f); lc_tenv = tenv; lc_orig = Cstr c; lc_id = None}, ho_index) in
  match f with
  | F.Fconstr (_, l, _, r, effect) ->
      ((List.map (make_wff env ho_index) l) @ (List.map (fun (p, fr) -> 
				(*let env = if (Lightenv.mem p env) then (Lightenv.remove p env) else env in*)
				(* We allow the effect to refer to return variable *)
				let env = Lightenv.addn [(Frame.returnpath, Builtins.uInt)] env in
				make_wff env ho_index fr) effect),
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Farrow (l, f, f', effect) ->
      let env' = match l with 
				| None -> 
					(Le.add (Frame.get_ho_param ho_index) f env)
				| Some p -> F.env_bind tenv env p f in
      ([make_wff env ho_index f; make_wff env' (ho_index+1) f'] @ 
			(List.map (fun (p, fr) -> 
				(*let env = if (Lightenv.mem p env) then (Lightenv.remove p env) else env in*)
				(* We allow the effect to refer to return variable *)
				let env = Lightenv.addn [(Frame.returnpath, Builtins.uInt)] env in
				make_wff env ho_index fr) effect), [])
  | F.Ftuple (fs, r) ->
      (List.map (make_wff env ho_index) fs, [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Frecord (_, fs, r) ->
      (List.map (fun (f',_,_) -> make_wff env ho_index f') fs,
       [(Cstr c, WFRef (Le.add qual_test_var f env, r, None))])
  | F.Fvar _ | F.Funknown ->
      ([],[]) 

let split cs =
  assert (List.for_all (fun c -> None <> c.lc_id) cs);
  C.expand 
    (fun (c, ho_index) -> match c.lc_cstr with SubFrame _ -> split_sub (c, ho_index) | WFFrame _ -> split_wf (c, ho_index))
    (List.map (fun c -> (c, 0)) cs) [] 

(**************************************************************)
(********************* Constraint Indexing ********************) 
(**************************************************************)

module WH = 
  Heap.Functional(
    struct 
      type t = subref_id * (int * bool * fc_id) 
      let compare (_,(i,j,k)) (_,(i',j',k')) = 
        if i <> i' then compare i i' else 
          if j <> j' then compare j j' else compare k' k
    end)

type ref_index = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;           (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  (* C.do_catch "get_rank" (SIM.find (get_ref_id c)) sri.rank *)
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,(_,qe),_,_) ->
  let ks = Le.fold (fun _ f l -> F.refinement_vars f @ l) env [] in
  match qe with F.Qvar (k, _) -> k::ks | _ -> ks

let rhs_k = function
  | SubRef (_,_,_,(_,F.Qvar (k, _)),_) -> Some k
  | _ -> None

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm VM.empty in
  let (dm,deps) = 
    SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, List.rev_append deps deps'))
      cm (SIM.empty,[]) in
  let flabel i = C.io_to_string ((SIM.find i om).lc_id) in
  let rm = 
    List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = (SIM.find id om).lc_id in
        SIM.add id (r,b,fci) rm)
      SIM.empty (C.scc_rank flabel deps) in
  (dm,rm)

let fresh_refc = 
  let i = ref 0 in
  fun c -> 
    let i' = incr i; !i in
    match c with  
    | WFRef (env,r,None) -> WFRef (env,r,Some i')
    | SubRef (env,g,r1,r2,None) -> SubRef (env,g,r1,r2,Some i')
    | _ -> assert false

(* API *)
let make_ref_index ocs = 
  let ics = List.map (fun (o,c) -> (o,fresh_refc c)) ocs in
  let (om,cm) = 
    List.fold_left 
      (fun (om,cm) (o,c) ->
        let o = match o with Cstr fc -> fc | _ -> assert false in
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = make_rank_map om cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig
(* API *)
let get_ref_deps sri c =
  let is' = try SIM.find (get_ref_id c) sri.depm with Not_found -> [] in
  List.map (get_ref_constraint sri) is'

(* API *)
let get_ref_constraints sri = 
  SIM.fold (fun _ c cs -> c::cs) sri.cnst [] 

(* API *)
let iter_ref_constraints sri f = 
  SIM.iter (fun _ c -> f c) sri.cnst

(* API *)
let push_worklist sri w cs =
  List.fold_left 
    (fun w c -> 
      let id = get_ref_id c in
      let _ = C.cprintf C.ol_solve "@[Pushing@ %d@\n@]" id in 
      if Hashtbl.mem sri.pend id then w else 
        let _ = Hashtbl.replace sri.pend id () in
        WH.add (id,get_ref_rank sri c) w)
    w cs

(* API *)
let pop_worklist sri w =
  try 
    let id = fst (WH.maximum w) in
    let _ = Hashtbl.remove sri.pend id in
    (Some (get_ref_constraint sri id), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let make_initial_worklist sri =
  let cs = List.filter is_subref_constraint (get_ref_constraints sri) in
  push_worklist sri WH.empty cs 

(**************************************************************)
(************************** Refinement ************************)
(**************************************************************)

module PM = Map.Make(struct type t = P.t let compare = compare end)
(*let pred_to_string p = Format.sprintf "@[%a@]" P.pprint p *)

let close_over_env cache env s ps =
  let rec close_rec clo = function
      | [] -> clo
      | ((P.Atom (P.Var x, P.Eq, P.Var y)) as p)::ps ->
          (try let tvar =
            if Path.same x qual_test_var then Some y else 
              if Path.same y qual_test_var then Some x else None in
          (match tvar with None -> close_rec (p :: clo) ps | Some t ->
            let ps' = F.conjuncts cache s qual_test_expr (Le.find t env) in
						let ps' = List.filter (fun p -> 
							not (p = Predicate.Atom (qual_test_expr, Predicate.Eq, Predicate.Var t) || 
							p = Predicate.Atom (Predicate.Var t, Predicate.Eq, qual_test_expr))
						) ps' in
            close_rec (p :: clo) (ps'@ps))
					with _ -> 
						(Format.fprintf Format.std_formatter "@.%a cannot be closed under env:%a@." 
						Predicate.pprint' p Frame.pprint_fenv env; 
						assert false))
      | p::ps -> close_rec (p :: clo) ps in
  close_rec [] ps 

(*
let qual_implied s lhs lhsm rhs_subs q =
  let rhs = F.refinement_predicate (solution_map s) qual_test_var (rhs_subs, F.Qconst [q]) in
  let (cached, cres) = if !Cf.cache_queries then TP.check_table lhs rhs else (false, false) in
  if cached then cres else 
    if (not !Cf.no_simple_subs) && PM.mem rhs lhsm then (incr stat_matches; true) else
      let rv = Bstats.time "refinement query" (TP.implies lhs) rhs in
      let _ = incr stat_imp_queries in
      let _ = if rv then incr stat_valid_imp_queries in
      rv 
*)
let implies_match env sm r1 = 
	let cache = Hashtbl.create 7 in
  let lhsm =
    Bstats.time "close_over_env" 
      (fun () ->
        List.fold_left (fun pm p -> PM.add p true pm) PM.empty 
        ((close_over_env cache env sm) (
					(** Here we remove higher order substitution *)
					let r1 = match r1 with
						| (subs, (Frame.Qvar (k, _) as refi)) -> 
							(List.filter (fun (p, pe) -> match pe with
								| Predicate.Var var -> List.for_all (fun ho -> 
									not (Path.same ho var)
									) (Frame.get_all_ho_params ())
								| _ -> true
								) subs, refi)
						| _ -> r1
					in F.refinement_conjuncts cache sm qual_test_expr r1))) () in
  fun (_,p) -> 
    let rv = (not !Cf.no_simple_subs) && PM.mem p lhsm in
    let _ = if rv then incr stat_matches in rv
		
(** Preserve predicates that are over existential quantifier *)		
let filter_exquan pred = 
	let preds = Predicate.split pred in
	let preds = List.filter (fun pred -> 
		let vars = Predicate.vars pred in 
		(List.exists (fun var -> (List.exists (fun ex -> (Path.same var ex) 
			) (Frame.get_all_ex_params ()))) vars)	
	) preds in
	Predicate.big_and preds
							
let search_accesses env = 		
	Common.remove_duplicates (Lightenv.fold (fun path fr res -> 
		(* For array get *)
		if (String.compare (Path.name path) "arraygetencoding" = 0) then match fr with
			(* Fixme. Include hoencoding *)
			| Frame.Fconstr (x,_,_,([], Frame.Qconst qs),_) -> 
				let pred = Predicate.big_and (List.map (fun (_,_,p) -> p) qs) in
				List.fold_left (fun res func -> match func with
					| Predicate.FunApp (c, args) ->
						if (Common.str_contains c "Ret") then
							(* Program construct like array/higher order function *)
							let path = match (List.hd args) with Predicate.Var path -> path | _ -> assert false in
							let args = List.tl args in
							if (List.for_all (fun arg -> List.for_all (fun var -> 
									List.for_all (fun ho -> not (Path.same ho var)) (Frame.get_all_ho_params ())) 
									(Predicate.exp_vars arg)) args) then (* Find array/higher order function access *)
								let (access,_) = List.fold_left (fun (res, i) arg -> (res@[(Frame.get_ho_param i, arg)], i+1)) ([], 0) args in
								res @ [(path, access)]
							else res
						else res
					| _ -> assert false 
				) res (Predicate.get_all_funs pred)
			| _ -> assert false
		(* For array update *)
		else (match fr with
			| Frame.Fconstr (x,_,_,(subs, Frame.Qconst qs),_) when x = Predef.path_array -> 
				let vs = List.map (fun (_,v,_) -> v) qs in 
				let pred = Predicate.apply_substs subs (Predicate.big_and (List.map (fun (_,_,p) -> p) qs)) in
				let access = ref [] in
				let _ = Predicate.map_pred (fun pred -> match pred with
					| Predicate.Atom (Predicate.Var e1, Predicate.Eq, Predicate.Var e2) -> 
						(let tvar = 
							if List.exists (fun ho -> Path.same ho e1) (Frame.get_all_ho_params ()) then
								Some (e1, Predicate.Var e2) 
							else if List.exists (fun ho -> Path.same ho e2) (Frame.get_all_ho_params()) then
								Some (e2, Predicate.Var e1)
							else None in
						match tvar with
							| Some tvar -> (access := (!access) @ [tvar]; pred)
							| None -> pred)
					| _ -> pred) pred in
				let paths = List.fold_left (fun res func -> match func with
					| Predicate.FunApp (c, args) when (Common.str_contains c "Ret") -> 
						let path = match (List.hd args) with Predicate.Var path -> path | _ -> assert false in
						if (List.exists (fun v -> Path.same v path) vs) then (res)
						else res @ [path]
					| _ -> res
				) [] (Predicate.get_all_funs pred) in
				if (List.length paths = 1) then res @ [(List.hd paths, !access)]
				else if (List.length paths = 0 && List.length (!access) = 0) then res
				else (Format.fprintf Format.std_formatter "working pred: %a@." Predicate.pprint pred; assert false)
			| _ -> res		
	)) env [])
	
(** To report invariants of sorted array some pattern may be needed
		1. Find the array sorting property 2. Find array get/set 3. generate the pattern *)
let sorted_pattern env is_higher_order sm =
	(* Fix me. Use a theorem prover to find sorting. Very simple implementation here *)
	let find_sort pred = match pred with
		| Predicate.Or (Predicate.Not (Predicate.And (low, high)), cons) -> (match cons with
			| Predicate.Atom (Predicate.FunApp ("Ret2", [Predicate.Var arr; Predicate.Var ho]), Predicate.Le, 
				Predicate.FunApp ("Ret2", [Predicate.Var arr'; Predicate.Binop (Predicate.PInt 1,Predicate.Plus,Predicate.Var ho')]))  
				when (Path.same arr arr' && Path.same ho ho' && List.exists (fun rho -> Path.same rho ho) (Frame.get_all_ho_params ())) -> 
					(match (low, high) with
					| (Predicate.Atom (low,Predicate.Le,_), Predicate.Atom (_,Predicate.Lt,high)) -> 
						Some (low, high)
					| _ -> None) 
			| _ -> None)
		| _ -> None in
	(** 1. Find the array sorting property *)
	let sorted_prop = Lightenv.fold (fun path fr res -> match fr with
		| Frame.Fconstr (x,_,_,(subs, Frame.Qvar (k,_)),_) when x = Predef.path_array ->
			(try let sol = sm k in
			let pred = Predicate.apply_substs subs (Predicate.big_and (List.map (fun (_,_,p) -> p) sol)) in
			let sorts = ref [] in
			(ignore (Predicate.map_pred (fun pred -> ((match (find_sort pred) with 
				| Some (low, high) -> (sorts:=(path, k, low, high)::(!sorts)) | _ -> ()); pred)) pred);
			res @ (!sorts)) with _ -> res)
		| _ -> res	
	) env [] in
	if (List.length sorted_prop > 0) then
		(** 2. Find array get/set *)
		let accesses = Hashtbl.create 3 in
		let _ = List.iter (fun (path, access) -> 
			if (Hashtbl.mem accesses path) then
				Hashtbl.replace accesses path ((Hashtbl.find accesses path)@(List.map snd access))
			else Hashtbl.replace accesses path (List.map snd access)	
		) (search_accesses env) in 
		(** 3. Generate the pattern *)
		let ho = Frame.get_ho_param 0 in
		List.fold_left (fun res (arr, arr_k, low, high) -> 
			if (Hashtbl.mem accesses arr) then
				let accesses = Hashtbl.find accesses arr in
				List.fold_left (fun res access -> 
					res @ [Predicate.implies (
						Predicate.And (Predicate.Atom (low, Predicate.Le, Predicate.Var ho), 
													Predicate.Atom (Predicate.Var ho, Predicate.Le, access)), 
						Predicate.Atom (Predicate.FunApp ("Ret2", [Predicate.Var arr; Predicate.Var ho]), Predicate.Le, 
														Predicate.FunApp ("Ret2", [Predicate.Var arr; access])))] 
					@ [Predicate.implies (
						Predicate.And (Predicate.Atom (access, Predicate.Le, Predicate.Var ho), 
													Predicate.Atom (Predicate.Var ho, Predicate.Le, high)), 
						Predicate.Atom (Predicate.FunApp ("Ret2", [Predicate.Var arr; access]), Predicate.Le, 
														Predicate.FunApp ("Ret2", [Predicate.Var arr; Predicate.Var ho])))]
				) res accesses	
			else res
		) [] sorted_prop
	else [] (* As there is no sorted property the above computation get simply eliminated *)
	
(** If deciding properties on array AA=array may be needed in some rare case *)
let look_checking_obj env r = 
	let r_k = match r with (ksubs, F.Qvar (k, _)) -> Some (ksubs, k) | _ -> None in
	let path_option_same p1 (p2subs, p2) = 
		match p1 with 
			| Some (p1subs, p1) -> 
				Path.same p1 p2 && (List.length p1subs = List.length p2subs) &&
				List.for_all2 (fun (a, b) (c, d) -> Path.same a c && b = d) p1subs p2subs 
			| _ -> false in 
	Lightenv.fold (fun path fr res -> match fr with
		| Frame.Fconstr (x,_,_,(pathsubs, Frame.Qvar (k,_)),_) when 
			(x = Predef.path_array || x = Predef.path_list || Hashtbl.mem !(Wellformed.measures) x) ->
			if (path_option_same r_k (pathsubs, k) (*&& List.for_all (fun u -> not (Path.same u k)) !functional_unknows*)) 
			then 
			((*match r_k with 
				| Some k -> 
					Format.fprintf Format.std_formatter "path = %s and k = %s@." (Path.name path) (Path.unique_name k)
				| None -> assert false*)
				res @ [path] )
			else res
		| _ -> res
	) env []
	
let lookup_array env r = 
	let cands = look_checking_obj env r in
	((*if (List.length cands > 1) then (
		
		List.iter (fun cand -> 
			match r_k with Some k -> Format.fprintf Format.std_formatter "path = %s and k = %s" (Path.name cand) (Path.unique_name k)
			| None -> assert false) cands
		; assert false); *)
	
	(*if (List.length cands = 1) then	
		List.map (fun cand -> Predicate.Atom (qual_test_expr, Predicate.Eq, Predicate.Var cand)) cands
	else [])*)
	if (List.length cands = 0) then [] 
	else [Predicate.big_and (List.map (fun cand -> Predicate.Atom (qual_test_expr, Predicate.Eq, Predicate.Var cand)) cands)])
	
let lookup_id env r id = 
	let cands = look_checking_obj env r in
	(*List.map (fun cand -> Predicate.Atom (id, Predicate.Eq, Predicate.Var cand)) cands*)
	if (List.length cands = 0) then []
	else [Predicate.big_and (List.map (fun cand -> Predicate.Atom (id, Predicate.Eq, Predicate.Var cand)) cands)]
	
(** Automatically instantiate the old array *)
let inst_oldarray env = 
	let arrays = Lightenv.mapfilter (fun p f -> match f with (* Find all arrays *)
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> Some p
		| _ -> None
	) env in
	let arraytable = Hashtbl.create 3 in
	let _ = List.iter (fun array -> 
		let name = Path.name array in
		if (Hashtbl.mem arraytable name) then
			Hashtbl.replace arraytable name ((Hashtbl.find arraytable name) @ [array])	
		else Hashtbl.replace arraytable name [array]
	) arrays in
	Hashtbl.fold (fun name arrays res -> 
		let arrays = List.sort (fun v1 v2 -> 
			let (v, v1, v2) = (Path.name v1, Path.unique_name v1, Path.unique_name v2) in
			let (v1, v2) = (String.sub v1 (String.length v+1) (String.length v1 - String.length v -1), 
											String.sub v2 (String.length v+1) (String.length v2 - String.length v -1)) in 	
			let (v1, v2) = (int_of_string v1, int_of_string v2) in
			Pervasives.compare v1 v2
		) arrays in	
		(assert (List.length arrays > 0); 
		let base = List.hd arrays in
		if (List.length arrays = 1) then	
			res @ [Predicate.Atom (Predicate.Var (Path.Pident (Ident.create_with_stamp (Frame.old_array_flag^(Path.name base)) (Path.stamp base))), 
				Predicate.Eq, Predicate.Var (List.hd arrays))]
		else res @ [Predicate.Atom (Predicate.Var (Path.Pident (Ident.create_with_stamp (Frame.old_array_flag^(Path.name base)) (Path.stamp base))), 
					Predicate.Eq, Predicate.Var (List.nth arrays 1))])
	) arraytable [] 

(** Instantiate the existential quantifiers in the goal predicate with previously refreshed EXs *)	
let inst_consequence_exquan arr_exquan env r p = 
	(** merge access with arr_exquan *)
	(*let _ = List.iter (fun (path, access) -> 
		if (Hashtbl.mem arr_exquan path) then
			Hashtbl.replace arr_exquan (Common.map_partial (access@(Hashtbl.find arr_exquan path)))
		else Hashtbl.replace arr_exquan path access
	) (search_accesses env) in*)
	let rec close identifications smallbindings =  
		let l = List.length identifications in
		let identifications = Common.remove_duplicates (List.fold_left (fun res (_, fr) -> match fr with
			| Frame.Fconstr (_, _, _, ([], Frame.Qconst [(_, _, 
						Predicate.Atom (Predicate.Var p1,Predicate.Eq,Predicate.Var p2))]), _) ->
				if (List.exists (fun p -> Path.same p p1) res) then res @ [p2]
				else if (List.exists (fun p -> Path.same p p2) res) then res @ [p1]
				else res
			| _ -> assert false
		) identifications (smallbindings)) in
		if (List.length identifications = l) then identifications
		else close identifications smallbindings in
	let functions = Predicate.get_all_funs p in
	let functions = Common.map_partial (fun fexpr -> match fexpr with
		| Predicate.FunApp (fn, args) when (Common.str_contains fn "Ret") -> 
			let vars = Predicate.exp_vars fexpr in
			if (List.exists (fun var -> Common.str_contains (Path.name var) "ex_") vars) 
			then Some (Predicate.exp_var (List.hd args)) else None 
		| _ -> None) functions in
	if (List.length functions = 1) then
		let arr = List.hd functions in
		let arrs = if (arr = qual_test_var) then look_checking_obj env r else [arr] in
		let possibles = 
			List.fold_left (fun res arr -> 
				if (Hashtbl.mem arr_exquan arr) then
					(** Search the array that is logicially identical *)
					let smallenv = (Lightenv.filter (fun key fr -> match fr with 
						| Frame.Fconstr (_,_,_,(_,(Frame.Qconst _)),_) -> String.compare (Path.name key) "fixarrayencoding" = 0
						| _ -> false
					) env) in
					let identifications = close [arr] (Lightenv.bindings smallenv) in 
					let subs = List.fold_left (fun res arr -> 
						let subs = Hashtbl.find arr_exquan arr in
						res @ subs
					) [] identifications in
					let subs = Common.remove_duplicates subs in
					res @ (List.map (fun sub -> 
						let sub = Misc.mapi (fun (_, b) i -> (Frame.get_ex_param i, b)) sub in
						Predicate.apply_substs sub p) subs)
				else res
			) [] arrs in
		let possibles = Common.remove_duplicates possibles in
		if (List.length possibles > 0) then Predicate.big_or possibles
		else if (not (Lightenv.mem Frame.refresh_flag env)) then  
			(** This is not inituitive: If the env is pure, then existential variables can be substituted by universal variables *)
			(Predicate.subst (Predicate.Var (Frame.get_ho_param 0)) (Frame.get_ex_param 0) p)
		else p
	else p

(** Find all array accessing with an existentially quantifier; replace them with refreshed EX variables; *)
let inst_premise_exquan lhs arr_exquan = 
	let preds = Predicate.split lhs in
	let (expreds, normalpreds) = List.partition (fun pred -> 
		let vars = Predicate.vars pred in
		List.exists (fun var -> Common.str_contains (Path.name var) "ex_") vars) preds in
	let inst_expreds = Common.map_partial (fun expred -> 
		let functions = Predicate.get_all_funs expred in
		let functions = Common.map_partial (fun fexpr -> match fexpr with
			| Predicate.FunApp (fn, args) when (Common.str_contains fn "Ret") -> 
				(let array = List.hd args in 
				match array with
					| Predicate.Var fvar when (not (fvar = qual_test_var)) -> 
						let (array, _) = (fvar, List.tl args) in
						let exvars = Common.remove_duplicates (
							List.find_all (fun var -> Common.str_contains (Path.name var) "ex_") (Predicate.exp_vars fexpr)) in
						if (List.length exvars > 0) then
							let _ = assert (not (array = qual_test_var)) in
							let subs = List.map (fun exvar -> (exvar, (Predicate.Var (newEX())))) exvars in
							let subs2 = Misc.mapi (fun (_, ex) i -> (Frame.get_ho_param i, ex)) subs in 
							Some (array, subs, subs2)
						else None
					| array -> None)
			| _ -> None) functions in
		if (List.length functions = 1) then
			let (array, subs, subs2) = List.hd functions in
			((if Hashtbl.mem arr_exquan array then 
				(Hashtbl.replace arr_exquan array ((Hashtbl.find arr_exquan array)@[(subs2)]))
			else Hashtbl.replace arr_exquan array [(subs2)]);
			Some (Predicate.apply_substs subs expred))
		else (assert (List.length functions = 0); None)
	) expreds in
	Predicate.big_and (normalpreds @ inst_expreds)
	
	
(** Find all array accessing with an existentially quantifier; replace them with refreshed EX variables; *)
let inst_premise_exquan_w_ex lhs arr_exquan fix_ex = 
	let preds = Predicate.split lhs in
	let (expreds, normalpreds) = List.partition (fun pred -> 
		let vars = Predicate.vars pred in
		List.exists (fun var -> Common.str_contains (Path.name var) "ex_") vars) preds in
	let inst_expreds = Common.map_partial (fun expred -> 
		let functions = Predicate.get_all_funs expred in
		let functions = Common.map_partial (fun fexpr -> match fexpr with
			| Predicate.FunApp (fn, args) when (Common.str_contains fn "Ret") -> 
				(let array = List.hd args in 
				match array with
					| Predicate.Var fvar when (not (fvar = qual_test_var)) -> 
						let (array, _) = (fvar, List.tl args) in
						let exvars = Common.remove_duplicates (
							List.find_all (fun var -> Common.str_contains (Path.name var) "ex_") (Predicate.exp_vars fexpr)) in
						if (List.length exvars > 0) then
							let _ = assert (not (array = qual_test_var)) in
							let subs = List.map (fun exvar -> (exvar, (Predicate.Var (fix_ex)))) exvars in
							let subs2 = Misc.mapi (fun (_, ex) i -> (Frame.get_ho_param i, ex)) subs in 
							Some (array, subs, subs2)
						else None
					| array -> None)
			| _ -> None) functions in
		if (List.length functions = 1) then
			let (array, subs, subs2) = List.hd functions in
			((if Hashtbl.mem arr_exquan array then 
				(Hashtbl.replace arr_exquan array ((Hashtbl.find arr_exquan array)@[(subs2)]))
			else Hashtbl.replace arr_exquan array [(subs2)]);
			Some (Predicate.apply_substs subs expred))
		else (assert (List.length functions = 0); None)
	) expreds in
	Predicate.big_and (normalpreds @ inst_expreds)	
		

(** All the refreshed existential variables should be matched to the ground terms for constraints *)	
let find_existential_pattern env is_higher_order sm arr_exquan = 
	(*For an array a that we have instantied its existential index 
		1. We search from array predicates that are existentially quantified and universally quantifed on a
	  2. We instantiate the universall index of a in such predicates to the instaniated index for a *)
	let patterns = Hashtbl.fold (fun arr subs res -> 
		let array_predicate = environment_predicate is_higher_order (Hashtbl.create 0) sm 
		(Lightenv.filter (fun k fr -> match fr with 
			| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> not (Path.same k arr)
			| _ -> false ) env) in
		let ex_array_predicate = filter_exquan array_predicate in 
		let ex_array_predicates = Predicate.split ex_array_predicate in
		let relevants = List.filter (fun pred -> 
			let vars = Predicate.vars pred in
			List.exists (fun var -> Path.same var arr) vars	
		) ex_array_predicates in
		if (List.length relevants > 0) then
			let env_predicate = Predicate.big_and relevants in	
			List.fold_left (fun res sub -> 
				res @ [(Predicate.apply_substs sub env_predicate)]	
			) res subs	
		else res
	) arr_exquan [] in
	let more_arr_exquan = Hashtbl.create 3 in
	let ex = newEX () in
	let patterns = List.map (fun pattern -> inst_premise_exquan_w_ex pattern more_arr_exquan ex) patterns in
	(** merge more_arr_exquan with arr_exquan *)
	(Hashtbl.iter (fun arr subs -> 
		if (Hashtbl.mem arr_exquan arr) then
			Hashtbl.replace arr_exquan arr ((Hashtbl.find arr_exquan arr) @ subs)
		else Hashtbl.replace arr_exquan arr subs
	) more_arr_exquan;
	patterns)	
	
(** Search through enviroment to find array (higher order function) get/set*)
let search_patterns_from_env env is_higher_order sm recorder = 
	let accesses = search_accesses env in
	List.fold_left (fun res (path, access) -> 
		(*let _ = Format.fprintf Format.std_formatter "path = %s@." (Path.name path) in
		let _ = List.iter (fun (a, b) -> Format.fprintf Format.std_formatter "access %s with %a@." 
							(Path.name a) Predicate.pprint_pexpr b) access in*)
		let env_predicate = 
			environment_predicate is_higher_order (Hashtbl.create 0) sm 
			(Lightenv.filter (fun k fr -> (*match fr with
				| Frame.Fconstr (x,_,_,(_, Frame.Qconst _),_) -> false
				| _ ->*) String.compare (Path.name k) (Path.name path) = 0) env) in
		let patterns = Predicate.apply_substs access env_predicate in
		let moreaccess = List.map (fun (a, b) -> (a, Predicate.Binop (b,Predicate.Minus,Predicate.PInt 1))) access in
		let morepatterns = Predicate.apply_substs moreaccess env_predicate in
		let more_recorder = Hashtbl.create 3 in
		let patterns = res @ [inst_premise_exquan patterns more_recorder] @ [ morepatterns ] in
		(Hashtbl.iter (fun arr subs -> 
			if (Hashtbl.mem recorder arr) then
				Hashtbl.replace recorder arr ((Hashtbl.find recorder arr) @ subs)
			else Hashtbl.replace recorder arr subs
		) more_recorder;
		patterns)
	) [] (accesses)	

(** To prove for-all properties we must do pattern matching *)				
let	pattern_match recorder env is_higher_order sm r r1p p = 
	(** overload the get_all_funs function that does not count uninterpreted functions from existential invairants *)
	let get_all_funs p = 
		let vars = Predicate.vars p in
		if (List.for_all (fun var -> (List.for_all (fun ex -> not (Path.same var ex) 
			) (Frame.get_all_ex_params ()))) vars) then	
			Predicate.get_all_funs p
		else [] in
	(******* For Decidable Checking *******)
	(** Find array identity *)
	let array_indetities = lookup_array env r in
	(** Find sorted pattern *)
	let sorted_patterns = sorted_pattern env is_higher_order sm in
	(*let _ = if (List.length sorted_patterns > 0) then 
		(List.iter (fun p -> Format.fprintf Format.std_formatter "sorted_pattern = %a@." Predicate.pprint p) sorted_patterns; 
		assert false) in*)
	(** Analyze p, if p contains function that is not normalized, 
	r1p is adapted to force normalization *)
	(* Fixme! In this version we only adapt r1p (excluding envp) *)
	let flag = List.for_all (fun v -> 
		List.for_all (fun ho -> not (Path.same v ho)) (Frame.get_all_ho_params ())) (Predicate.vars r1p) in
	(* Fixme! Flag only dectects whether the pattern generated can be matched to r1p *)
	let patterns = if flag then [] else List.fold_left (fun res func -> match func with
		| Predicate.FunApp (c, args) when (* Fixme! In this version we only adapt r1p *)
			(List.exists (fun arg -> List.exists (fun var -> Path.same var qual_test_var) (Predicate.exp_vars arg)) args) -> 
			if (Common.str_contains c "Ret") then 
				(*Invariants inferred from our side may need to be normalized*)
				let args = Common.map_partial (fun arg -> match arg with
					| Predicate.Var arg -> 
						if (List.exists (fun ho -> Path.same ho arg) (Frame.get_all_ho_params ())) then
							None (* Normalized already *)
						else if (Path.same arg qual_test_var) then None
						(* Fixme. We only deal with one-dimensional array *)
						else Some ((Frame.get_ho_param 0), Predicate.Var arg)
					| _ -> (* Need to adapt r1p to normalize this *)
						let vars = Predicate.exp_vars arg in
						if (List.exists (fun var -> 
							List.exists (fun ho -> Path.same ho var) (Frame.get_all_ho_params ())) vars) then
							let ho = List.find (fun ho -> 
								List.exists (fun var -> Path.same ho var) vars) (Frame.get_all_ho_params ()) in
							Some (ho, arg)
						(* Fixme. We only deal with one-dimensional array *)
						else Some (Frame.get_ho_param 0, arg)
					) args in
				if (List.length args != 0) then res @ [args] else res 
			else res (*Invariants from client are assumed to be normalized*)
		| _ -> (* Fixme. We leave how to adapt envp in the future *) 
			res) [] (get_all_funs p) in
	let patterns = Common.remove_duplicates patterns in
	let r1p_patterns = List.map (fun pattern -> Predicate.apply_substs pattern r1p) patterns in
	(** We additional find more patterns from the arguments to array (higher order function) get/set *)
	(*let _ = List.iter (fun p -> Format.fprintf Format.std_formatter "r1p_pattern = %a@." Predicate.pprint p) r1p_patterns in*)
	let more_patterns = if (*(List.length r1p_patterns = 0) *)flag
											then [] else search_patterns_from_env env is_higher_order sm recorder in
	(*let _ = List.iter (fun p -> Format.fprintf Format.std_formatter "more_pattern = %a@." Predicate.pprint p) more_patterns in*)
	let old_arrays = inst_oldarray env in
	let ex_patterns = find_existential_pattern env is_higher_order sm recorder in
	array_indetities @ sorted_patterns @ r1p_patterns @ more_patterns @ old_arrays @ ex_patterns


(*let cached_environment_predicate is_higher_order sm env = 
	P.big_and (
		Common.map_partial (fun x -> x) (Le.maplist (fun v f -> 
				match f with
					| Frame.Farrow _ -> 
						if (is_higher_order v f) then 
							let pred = (F.predicate sm (P.Var v) f) in
							Some pred
						else None
					| _ -> Some (F.predicate sm (P.Var v) f) 
		) env)
	)*)
				
(** Major Design Update: Local type inference is omitted since local information is encoded into verification condition *)	
(** Parameter sri --> the load of all typing constraints *)	
(** Parameter envtable --> Enviromental variable should not be duplicated in verification condition *)
(** Parameter k --> Unknown local type identifier *)
(** return --> refinement predicate for the unknown local type labeled by k (such type could be ill-formed but ok) *)
let rec local_types is_higher_order sri sm cache id k = 
	(** Find the constraint for k --> generate its refinement types *)
  let cs = get_ref_constraints sri in
	let cs = List.filter (fun c -> 
		let rk = rhs_k c in 
		match rk with
			| Some rk -> Path.same rk k
			| None -> false) cs in
	let (ps, caches') = List.fold_left (fun (res, rescache) c -> match c with
		| SubRef (env,g,r1, (sub2s, F.Qvar _), _) ->
			let gp = Bstats.time "make guardp" (guard_predicate ()) g in
			let cache' = Hashtbl.copy cache in
    	let envp = Bstats.time "make envp" (environment_predicate is_higher_order cache' sm) env in
    	let r1p = Bstats.time "make r1p" (F.refinement_predicate cache' sm qual_test_expr) r1 in
			let wrapps = P.big_and (lookup_id env r1 id) in
			((P.big_and [envp;gp;r1p; wrapps; wrapps]) :: res, (Common.hashkeys (cache')) :: rescache)
		| _ -> (res, rescache)
	) ([], []) cs in
	let keys = Common.find_sharing caches' (Path.same) in
	let _ = List.iter (fun key -> Hashtbl.replace cache key ()) keys in 
	if (List.length ps = 0) then [(Path.mk_ident "", Path.mk_ident "", Predicate.True)]
	else 
		[(Path.mk_ident "", (*Path.mk_ident ""*)qual_test_var, Predicate.big_or ps)]
								
(** We deal with quantified variables; the variables stored in instvars can be used
		to instantiate for-all quantified formulas *)								
let implies_tp is_higher_order instvars env g sm r1 = 
	let cache = Hashtbl.create 7 in
  let (r1p, lhs) = 
    let gp = Bstats.time "make guardp" (guard_predicate ()) g in
    let envp = Bstats.time "make envp" (environment_predicate is_higher_order cache sm) env in
    let r1p = Bstats.time "make r1p" (F.refinement_predicate cache sm qual_test_expr) r1 in
    (r1p, P.big_and [envp;gp;r1p]) in
	(** We need to instantiate existential variables; the instantiated variables should also be used to generate patterns *)
  (*1. Define a recorder that remember how the existential variable are instantied for each array (to a unique EX variable) *)
	let recorder = Hashtbl.create 3 in
	(*2. Instantiate the existential variables  *)
	let lhs = inst_premise_exquan lhs recorder in 
	let lhs = Predicate.deduct_datastructure_proof lhs in
	let lhs = Predicate.inst_datastruture_proof lhs instvars in
	(** Deal with (non-array) for-all quantifiers **)
	let lhs = Predicate.inst_forall lhs [forall_uexpr; forall_vexpr] in
	let ch = Bstats.time "TP implies" TP.implies lhs in
  fun (_,p) -> 
		(*3. pattern_match should also instantiate universal quantifiers with the instantiation to exisetential variables *)
		let recorder = Hashtbl.copy recorder in
		let wrapps = pattern_match recorder env is_higher_order sm r1 r1p p in
		(*let _ = Format.fprintf Format.std_formatter "List.length wrapps = %d@." (List.length wrapps) in*)
		(*let _ = List.iter (fun wrapp -> Format.fprintf Format.std_formatter "wrap = %a@." Predicate.pprint' wrapp) wrapps in*)
		(******* For Decidable Checking Wrapps Must be Provided to SMT aw well *******)
		let _ = if (List.length wrapps > 0) then (TheoremProver.push (P.big_and wrapps)) in
		(*4. goal predicate should also consider how its existential variables can be instantiated *)
		let p = inst_consequence_exquan recorder env r1 p in
		(*let _ = Format.fprintf Format.std_formatter "goal pred = %a@." Predicate.pprint' p in *)
		let p = Predicate.inst_forall p [forall_uexpr; forall_vexpr] in
		(*let _ = Format.fprintf Format.std_formatter "inst goal pred = %a@." Predicate.pprint' p in*)
		let r = Bstats.time "ch" ch p in
		let _ = if (List.length wrapps > 0) then TheoremProver.pop () in
		(*let _ = 
			if (not r) then 
				Format.fprintf Format.std_formatter "%a --> %a Failed@."
				Predicate.pprint' lhs Predicate.pprint' p
			else 
				Format.fprintf Format.std_formatter "%a --> %a Succeed@."
				Predicate.pprint' lhs Predicate.pprint' p in*)
		(*let _ = (* Debug only *)
			match p with 
			| Predicate.Or _ -> 
				if (not r) then
				Format.fprintf Format.std_formatter "%a --> %a Failed@."
				Predicate.pprint' lhs Predicate.pprint' p
				else 
				Format.fprintf Format.std_formatter "%a --> %a Succeed@."
				Predicate.pprint' lhs Predicate.pprint' p
			| _ -> (if (List.length wrapps > 0) then 
				(Format.fprintf Format.std_formatter "%a result is %b@." Predicate.pprint' p r)) in*)
		r
		 
let refine_simple is_higher_order env g s k1 k2 =
	let sm = solution_map s in
  let q1s  = try sm k1 with _ -> [] in
  let q2s  = try sm k2 with _ -> [] in
  let (q2s',qp2s) = List.partition (fun q -> 
		(* Debug only *)
		let result = List.mem q q1s in
		(*let _ = if (not result) then match q with (_,_,p) ->  (
		match p with 
			| Predicate.Or _ -> Format.fprintf Format.std_formatter "%a is dropped because of not matching@."
				Predicate.pprint p
			| _ -> ();
		) in*)
		result
		) q2s in
	(** == Original implementation of liquid type do have the following code 
		However we find if missing, the implementation is buggy
		Also we wonder should we check all qualifiers rather than just disjunctions 
		which are inferred by us? == *)
	(*let qp2s = List.filter (fun q -> match q with (_,_,Predicate.Or _)-> true| _ -> false) qp2s in*)
	
  let q2s'' = 
		if (List.length qp2s > 0) then
			let qp2s = 
				List.map 
				  (fun q -> (q,F.refinement_predicate (Hashtbl.create 0) sm qual_test_expr ([],F.Qconst[q]))) 
				  qp2s in
			let instvars = Predicate.find_instantiable_variables (List.map snd qp2s) in
		  let tpc = Bstats.time "implies_tp" (implies_tp is_higher_order instvars env g sm) ([], F.Qvar (k1, F.Top)) in
		  let (qp2s2,_)    = Bstats.time "imp check" (List.partition tpc) qp2s in
		  let _ = Bstats.time "finish" TP.finish () in
			List.map fst qp2s2
		else [] in
	let q2s' = q2s'@q2s'' in
	(* == *)	
  let _    = Sol.replace s k2 q2s' in
  let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length q2s) (List.length q2s') in
  List.length q2s' <> List.length q2s	

let qual_wf sm env subs q =
  let result = refinement_well_formed env sm (subs,F.Qconst [q]) qual_test_expr in
	(* Debug only *)
	let _ = if (not result) then match q with (_,_,p) ->  (
		(*if (List.exists (fun pv -> (List.exists (fun ev -> Path.same pv ev) (Frame.get_all_ex_params ()))) (Predicate.vars p)) then (
			Format.fprintf Format.std_formatter "Env: ";
			List.iter (fun d -> Format.fprintf Format.std_formatter "%s " (Path.name d)) (Lightenv.domain env);
			Format.fprintf Format.std_formatter "@.";*)
			(*Format.fprintf Format.std_formatter "Eliminating %a or %a@." Predicate.pprint' p Predicate.pprint' (Predicate.apply_substs subs p) *)
			(* ) *)
		) in
	result
	
let rec error_rec cstr c = match cstr.lc_orig with
  | Loc loc -> 
    begin match cstr.lc_cstr with
      | SubFrame _ -> (
				Format.fprintf Format.std_formatter "At (SUB) %a for the constraint%a@." Location.print loc
					(pprint_ref None) c
			)
      | WFFrame (_,f) -> (
				Format.fprintf Format.std_formatter "At (WF) %a for the constraint%a@." Location.print loc
					(pprint_ref None) c
			)
    end
  | Assert loc -> Format.fprintf Format.std_formatter "At (ASSERT) %a@." Location.print loc
  | Cstr cstr -> error_rec cstr c

let refine is_higher_order sri s c =
  let _ = incr stat_refines in
  let sm = solution_map s in 
  match c with
  | SubRef (env, g, ([], F.Qvar (k1, _)), ([], F.Qvar (k2, _)), _)
    when not (!Cf.no_simple || !Cf.verify_simple) -> 
			if (not (Sol.mem s k2)) then true else
      let _ = incr stat_simple_refines in
      let refineresult = Bstats.time "refine_simple" (refine_simple is_higher_order env g s k1) k2 in
			(*let _ = if (refineresult) then error_rec (get_ref_orig sri c) c in*)
			refineresult
  | SubRef (env,g,r1, (sub2s, F.Qvar (k2, _)), _)  ->
			if (not (Sol.mem s k2)) then true else
      let _ = incr stat_sub_refines in
      let qp2s = 
        List.map 
          (fun q -> (q,F.refinement_predicate (Hashtbl.create 0) sm qual_test_expr (sub2s,F.Qconst[q]))) 
          (try sm k2 with _ -> []) in		
      let (qp2s1,qp2s') = Bstats.time "match check" (List.partition (implies_match env sm r1)) qp2s in
      let (qp2s2,_)    = 
				if (List.length qp2s' > 0) then 
					let instvars = Predicate.find_instantiable_variables (List.map snd qp2s') in
					let tpc = Bstats.time "implies_tp" (implies_tp is_higher_order instvars env g sm) r1 in
					let res = Bstats.time "imp check" (List.partition tpc) qp2s' in
					let _ = Bstats.time "finish" TP.finish () in res
				else ([], []) in
      let q2s'' = List.map fst (qp2s1 @ qp2s2) in
      let _ = Sol.replace s k2 q2s'' in
      let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length qp2s) (List.length q2s'') in
      let _ = stat_imp_queries := !stat_imp_queries + (List.length qp2s) in
      let _ = stat_valid_imp_queries := !stat_valid_imp_queries + (List.length q2s'') in
      let refineresult = (List.length qp2s  <> List.length q2s'') in
			(*let _ = if (refineresult) then error_rec (get_ref_orig sri c) c in*)
			refineresult
  | WFRef (env,(subs, F.Qvar (k, _)),_) ->
			if (not (Sol.mem s k)) then false else
      let _ = incr stat_wf_refines in
      let qs  = try solution_map s k with _ -> [] in
      let qs' = List.filter (qual_wf sm env subs) qs in
      let _   = Sol.replace s k qs' in
      let wfresult = (List.length qs <> List.length qs') in
			(*let _ = if (wfresult) then error_rec (get_ref_orig sri c) c in*)
			wfresult
  | _ -> false


(**************************************************************)
(********************** Constraint Satisfaction ***************)
(**************************************************************)

let sat is_higher_order s = function
	| SubRef (env, g, r1, (_, F.Qvar (k, _)), _) when (not (Sol.mem s k)) -> true
  | SubRef (env, g, r1, r2, _)  ->
			let cache = Hashtbl. create 7 in
      let gp = Bstats.time "make guardp" (guard_predicate ()) g in
      let envp = environment_predicate is_higher_order cache (solution_map s) env in
      let p1 = F.refinement_predicate cache (solution_map s) qual_test_expr r1 in
      let p2 = F.refinement_predicate cache (solution_map s) qual_test_expr r2 in
			(* For the same reason in iterative solving we do pattern matching for proving for-all properties *)
			let recorder = Hashtbl.create 3 in
			let lhs = P.big_and [envp; gp; p1] in
			let lhs = inst_premise_exquan lhs recorder in 
			let lhs = Predicate.deduct_datastructure_proof lhs in
			let lhs = Predicate.inst_datastruture_proof lhs (Predicate.find_instantiable_variables [p2]) in
			let lhs = Predicate.inst_forall lhs [forall_uexpr; forall_vexpr] in
			let wrapps = pattern_match recorder env is_higher_order (solution_map s) r1 p1 p2 in
			(*let _ = List.iter (fun wrapp -> Format.fprintf Format.std_formatter "wrap = %a@." Predicate.pprint' wrapp) wrapps in*)
			let lhs = if (List.length wrapps > 0) then P.big_and (lhs::wrapps) else lhs in
			(** Existenially quantifed invariants are checked individually *)
			let (exgoals, normalgoals) = List.partition (fun pred -> 
				let vars = Predicate.vars pred in
				List.exists (fun var -> Common.str_contains (Path.name var) "ex_") vars) (Predicate.split p2) in
			let goals = (Predicate.big_and normalgoals)::exgoals in
			let rv = List.for_all (fun goal -> 
				let goal = inst_consequence_exquan recorder env r1 goal in
				let goal = Predicate.inst_forall goal [forall_uexpr; forall_vexpr] in
      	let rv = TP.implies lhs goal in 
				let _ = TP.finish () in
				(*let _ = if (not rv) then 
				(Format.fprintf Format.std_formatter "env is %a@." (pprint_env_pred None) env;	
				Format.fprintf Format.std_formatter "Find an UNSAT: %a => %a --> %a SAT@." P.pprint' envp P.pprint' lhs P.pprint' goal) in*)
				rv
			) goals in
			(*let _ = if (not rv) then 
				(
				let goals = Predicate.split goal in
				List.iter (fun g -> 
					let result = TP.implies lhs goal in 
					let _ = TP.finish () in
					if (not result) then
						Format.fprintf Format.std_formatter "UNSAT CONST: %a => %a --> %a SAT@." P.pprint' envp P.pprint' p1 P.pprint' g
				) goals) in*)
			rv
  (*| WFRef (env,((subs,F.Qvar k) as r), _) as c -> 
      let rv = refinement_well_formed env (solution_map s) r qual_test_expr in
      C.asserts (Printf.sprintf "ERROR: wf is unsat! (%d)" (get_ref_id c)) rv;
      rv *)
  | _ -> true

let unsat_constraints is_higher_order sri s =
  C.map_partial
    (fun c -> if (sat is_higher_order s c) then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(**************************************************************)
(************************ Initial Solution ********************)
(**************************************************************)

(** If a type variable is for user defined data structure, we 
 *  will set it to the data structure invariant	
 *)
(** Find type variables for a user-defined data structure in a frame *)	
let find_dty_unknows_frame measures tbl b f = match f with
	| Frame.Fconstr (path, ts, vs, (_, Frame.Qvar (k, _)), eff) when (Hashtbl.mem measures path) -> 
		((if (Hashtbl.mem tbl k) then (Hashtbl.replace tbl k (b::(Hashtbl.find tbl k)))
		else Hashtbl.replace tbl k [b]); f) 
	| f -> f

let find_dty_unknows_env measures env tbl = 
	Lightenv.iter (fun _ f -> ignore (find_dty_unknows_frame measures tbl true f)) env
	
(** Find all type variables for user-defined data structure *)
let find_all_dty_unknows measures cs = 
	let tbl = Hashtbl.create 13 in
	let _ = List.iter (fun c -> (match c.lc_orig with
		| Cstr _ -> assert false
		| _ ->
			let cstr = c.lc_cstr in 
			match cstr with
				| SubFrame (env, g, f1, f2) -> 
					(find_dty_unknows_env measures env tbl;
					ignore (Frame.map (find_dty_unknows_frame measures tbl true) f1);
					ignore (Frame.map (find_dty_unknows_frame measures tbl false) f2))
				| WFFrame (env, f) -> ()
					(*(find_dty_unknows_env measures env tbl; 
					ignore (Frame.map (find_dty_unknows_frame measures tbl) f))*)
	)) cs in
	Hashtbl.fold (fun k bs res -> 
		if (List.for_all (fun b -> b) bs) then k::res
		else res) tbl []

let filter_dtyinv qs = 
	let measures = Hashtbl.fold (fun m _ res -> m::res) !(Wellformed.measures) [] in
	List.filter (fun (name, _, _) -> 
		let name = Path.name name in
		let name = String.lowercase name in	
		List.exists (fun m -> Common.str_contains name (Path.name m)) measures	
	) qs 
	
let make_dtyinv cs qs unknows s = 
	let qs = filter_dtyinv qs in
	let ks = find_all_dty_unknows !(Wellformed.measures) cs in
	List.iter (fun k -> 
		if (List.exists (fun u -> Path.same u k) unknows)	then ()
		else 
			(Sol.replace s k qs)
	) ks

(* If a variable only ever appears on the left hand side, the variable is
 * unconstrained; this generally indicates an uncalled function.
 * When we have such an unconstrained variable, we simply say we don't
 * know anything about its value.  For uncalled functions, this will give
 * us the type which makes the least assumptions about the input. *)

let make_initial_solution cs sri qs unknows =
  let s = Sol.create 17 in
  let addrv = function
  | ((_, F.Qconst _),_) -> ()
  | ((_, F.Qvar (k, F.Top)),false) -> 
		if not (Sol.mem s k) && (List.exists (fun unknow -> Path.same unknow k) unknows) then Sol.replace s k []
  | ((_, F.Qvar (k, _)),_) -> 
		if (List.exists (fun unknow -> Path.same unknow k) unknows) then
		(*let qs = List.filter (fun (_,_,p) -> match p with
			| Predicate.Atom (_,_,Predicate.PInt i)
			| Predicate.Atom (Predicate.PInt i,_,_) -> (i = 0)
			| Predicate.Atom (Predicate.Binop (Predicate.PInt i,Predicate.Minus,Predicate.PInt j),_,_)
			| Predicate.Atom (_,_,Predicate.Binop (Predicate.PInt i,Predicate.Minus,Predicate.PInt j)) -> (i = 0) && (j <= 1) && (j > 0)
			| _ -> true
			) qs in*)
		Sol.replace s k qs in
  SIM.iter 
    (fun _ c -> match c with 
    | SubRef (_, _, r1, r2, _) -> addrv (r1,false); addrv (r2,true)
    | WFRef (_, r, _) -> addrv (r,false)) sri.cnst;
	let _ = make_dtyinv cs qs unknows s in
  s

(** horefs is actually hoparamrefs *)	
let make_further_solution cs sri spec qs s horefs horeturnrefs unknows unknowreturns = 
	let cache = Hashtbl.create 13 in
  let addrv = function
  | ((_, F.Qconst _),_) -> ()
  | ((_, F.Qvar (k, F.Top)),false) -> ()
  | ((_, F.Qvar (k, _)),_) -> (* test if a qualifier is forbided *)
		(*let qs = List.filter (fun (ks,q) -> not (List.exists (fun k' -> Path.same k' k) ks)) qs in*)
		
		(* if k in horefs then set this qualifer for k otherwise do not set *)
		if (List.exists (fun unknow -> Path.same unknow k) unknows && not (Hashtbl.mem cache k)) then
		let _ = Hashtbl.replace cache k () in
		let qs = Bstats.time "filter1" (List.filter (fun (k', q) -> match k' with
			| Some k' ->
				if (List.exists (fun horef -> Path.same horef k') horefs) then true
				else 
					if (List.exists (fun horef -> Path.same horef k) horefs) then (*false*)
						match q with (_,_,p) ->
						if (List.exists (fun pv -> (List.exists (fun hov -> Path.same pv hov) (Frame.get_all_ho_params ()))) (Predicate.vars p))
						then false else true
					else 
						(* if q is set on a higher order functional post then this k must also be on a higher order post *) 
						(*if (List.exists (fun horetref -> Path.same horetref k') horeturnrefs) then
							List.exists (fun horetref -> Path.same horetref k) horeturnrefs
						else true*)
						(match q with 
							| (_,_,p) -> 
							if (List.exists (fun pv -> (List.exists (fun hov -> Path.same pv hov) (Frame.get_all_ho_params ()))) (Predicate.vars p)) then 
								(List.exists (fun horetref -> Path.same horetref k) horeturnrefs)
							else true	)
			| None -> true
		)) qs in
		let qs = List.map snd qs in
		let qs = 
			if !(Clflags.no_hoflag) then qs 
			else Bstats.time "removedups" (Common.remove_customized_duplicates (Qualifier.equals)) qs in
		let pre_qs = Sol.find s k in
		(* only want new qualifiers to be added; and, more importantly, pre- and post-invairant are not mixed *)
		let qs = Bstats.time "filter2" (List.filter (fun q -> 
			let qname = Path.name (match q with (p,_,_) -> p) in
			let pre = Str.regexp_string "Pre" in
			let post = Str.regexp_string "Post" in
			let process = 
				if not !(Clflags.reachability) then true else (*Fix me!!*)
				if (try ignore (Str.search_backward pre qname (String.length qname - 1)); true 
						with Not_found -> false) then
					List.for_all (fun u -> not (Path.same u k)) unknowreturns
				else if (try ignore (Str.search_backward post qname (String.length qname - 1)); true 
						with Not_found -> false) then
					List.exists (fun u -> Path.same u k) unknowreturns
				else true in
			process &&
				List.for_all (fun pre_q -> match (q, pre_q) with
				| ((_,valu,p), (_,valu',p')) -> ((Predicate.subst (Predicate.Var valu') valu p) <> p')
				) pre_qs
			)) (qs@spec) in
		Sol.replace s k (pre_qs @ qs) in
  SIM.iter 
    (fun _ c -> match c with 
    | SubRef (_, _, r1, r2, _) -> addrv (r1,false); addrv (r2,true)
    | WFRef (_, r, _) -> addrv (r,false)) sri.cnst;
	let _ = make_dtyinv cs spec unknows s in		
  s

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)
 
let dump_constraints sri = 
  if !Cf.dump_constraints then
  (printf "@[Refinement Constraints@.@\n@]";
  iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c))

let dump_solution_stats s = 
  let kn  = Sol.length s in
  let (sum, max, min) =   
    (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
     Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
     Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
  C.cprintf C.ol_solve_stats "@[Quals:@\n\tTotal:@ %d@\n\tAvg:@ %f@\n\tMax:@ %d@\n\tMin:@ %d@\n@\n@]"
  sum ((float_of_int sum) /. (float_of_int kn)) max min;
  print_flush ()
  
let dump_solving qs sri s step =
  if step = 0 then 
    let cs = get_ref_constraints sri in 
    let qn  = List.length qs in
    let kn  = Sol.length s in
    let wcn = List.length (List.filter is_wfref_constraint cs) in
    let rcn = List.length (List.filter is_subref_constraint cs) in
    let scn = List.length (List.filter is_simple_constraint cs) in
    (dump_constraints sri;
     C.cprintf C.ol_solve_stats "@[%d@ instantiated@ qualifiers@\n@\n@]" qn; 
     C.cprintf C.ol_solve_stats "@[%d@ variables@\n@\n@]" kn;
     C.cprintf C.ol_solve_stats "@[%d@ total@ quals@\n@\n@]" (kn * qn); 
     C.cprintf C.ol_solve_stats "@[%d@ split@ wf@ constraints@\n@\n@]" wcn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ subtyping@ constraints@\n@\n@]" rcn;
     C.cprintf C.ol_solve_stats "@[%d@ simple@ subtyping@ constraints@\n@\n@]" scn;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (C.cprintf C.ol_solve_stats "@[Refine Iterations: %d@ total (wf=%d,si=%d,su=%d)\n@\n@]" 
       !stat_refines !stat_wf_refines !stat_simple_refines !stat_sub_refines;
     C.cprintf C.ol_solve_stats "@[Implication Queries: %d@ total;@ %d@ valid;@ %d@ match@]@.@." 
       !stat_imp_queries !stat_valid_imp_queries !stat_matches;
     TP.print_stats ();
     dump_solution_stats s;
     flush stdout)

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

let rec solve_sub is_higher_order sri s w = 
  let _ = if !stat_refines mod 100 = 0 then C.cprintf C.ol_solve "@[num@ refines@ =@ %d@\n@]" !stat_refines in
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let (r,b,fci) = get_ref_rank sri c in
    let _ = C.cprintf C.ol_solve "@[Refining:@ %d@ in@ scc@ (%d,%b,%s):@]"
            (get_ref_id c) r b (C.io_to_string fci) in
    let w' = if Bstats.time "refine" (refine is_higher_order sri s) c then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub is_higher_order sri s w'

let solve_wf is_higher_order sri s = 
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine is_higher_order sri s c) | _ -> ())
	
let remove_nonalpha = Str.global_replace (Str.regexp "[^a-zA-Z]+") "";;

(** A user qualifier could specify 
	1) data structure inv 
	2) function's atomic predicates -- of which a functional invariant is composed
	3) arbitrary predicates -- randomly specified *)		
let userqs atomics qs fs = 
	(* Split the qs into { 1) + 3) } and { 2) } *)
	let (spec, atomicpreds) = List.partition (fun (name, _, p) -> 
		let name = String.lowercase (Path.name name) in
		List.for_all (fun f -> not (Common.str_contains name (Path.name f))) fs
		) qs in
	let _ = (List.iter (fun ((name, _, p)) -> 
		let name = Path.name name in
		let name = String.lowercase name in	
		let name = remove_nonalpha name in
		if (Hashtbl.mem atomics name) then
			Hashtbl.replace atomics name (Hashtbl.find atomics name @ [p])
		else Hashtbl.replace atomics name [p]
	) atomicpreds) in
	(spec, spec)

(* Use is_higher_order to exclude encoding for named functions *)
(* qs is the provided qualifiers while qs' are inferred *)
let solve functions is_higher_order query_atomics query_pos_samples query_neg_samples learn_from_samples gen_inv qs cs = 
	let atomics = Hashtbl.create 7 in
	let (qs, spec) = userqs atomics qs functions in
	let horefs = ho_param_refinements cs in
	let horeturnrefs = ho_return_refinements cs in
	let (unknows, unknowreturns) = f_params_return_refinements cs in
	let _ = functional_unknows := unknows in
	(*let _ = List.iter (fun u -> Format.fprintf Format.std_formatter "unknow = %s@." (Path.unique_name u)) unknows in*)
	let arrayrefs = array_refinements cs in
	let horeturnrefs = horeturnrefs @ arrayrefs in
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let sri = make_ref_index (split cs) in
  let s = make_initial_solution cs sri qs unknows in
	(*let _ = Sol.iter (fun k qs -> Format.fprintf Format.std_formatter "k as %s = qs@." (Path.unique_name k)) s in*)
  let _ = dump_solving qs sri s 0  in 
  let _ = Bstats.time "solving wfs" (solve_wf (fun _ _ -> false) sri) s in
	(*let _ = Sol.iter (fun k qs -> Format.fprintf Format.std_formatter "first k as %s = qs@." (Path.unique_name k)) s in*)
  let _ = dump_solving qs sri s 1 in
  let w = make_initial_worklist sri in
	(** Important: Register local type *)
	let _ = Frame.register_unfound (local_types is_higher_order sri (solution_map s)) in
  let _ = Bstats.time "solving sub" (solve_sub (fun _ _ -> false) sri s) w in
	(*let _ = Sol.iter (fun k qs -> Format.fprintf Format.std_formatter "second k as %s = qs@." (Path.unique_name k)) s in*)
  let _ = dump_solving qs sri s 2 in
  let _ = TP.reset () in
	(*let _ = Sol.iter (fun k qs -> Format.fprintf Format.std_formatter "third k as %s = qs@." (Path.unique_name k)) s in*)
  let _ = Format.fprintf Format.std_formatter "###########Validation############@." in
	let unsat = Bstats.time "testing solution" (unsat_constraints (fun _ _ -> false) sri) s in
	let count_samples neg_samples = 
		Hashtbl.fold (fun _ {spre = presamples; spost = postsamples} res -> 
			res + List.length presamples + List.length postsamples
			) neg_samples 0 in 
	(** An iterative solving procedure to incrementally build valid samples *)
	let array_inv_tried = ref (not (List.length arrayrefs > 0)) in
	let eager_termination = ref false in
	let _ = if (!array_inv_tried) then (Clflags.gen_inv := false) in
	let iter_count = ref 1 in
	let rec loop pos_samples prev_neg_samples prev_failed_invariant allquals unsat prev_s = 
		let _ = (iter_count := (!iter_count) + 1) in
		let _ = Format.fprintf Format.std_formatter "%d-ith incremental iteration...@." (!iter_count) in
		if (!eager_termination) then (unsat, prev_s, prev_failed_invariant) else
		let neg_samples = Bstats.time "query_neg_samples" 
				query_neg_samples prev_failed_invariant pos_samples in 
		if ((not !Clflags.reachability && count_samples neg_samples = 0) && (not !Clflags.gen_inv)) 
				|| (!iter_count >= 5 && not !(Backwalker.hoflag)) 
		(* if hoflag is set, iteratively generate more negative samples *)
		then 
			(unsat, prev_s, prev_failed_invariant)	
		else
			let _ = 
				Hashtbl.iter (fun path s ->
					if (Hashtbl.mem prev_neg_samples path) then
						let record = Hashtbl.find prev_neg_samples path in 
						Hashtbl.replace neg_samples path (
							{spre = record.spre @ s.spre;
							spost = record.spost @ s.spost}
							)	
				) neg_samples in
			let (invariants, qualifiers) = 
				if (!array_inv_tried) then
					let atomics' = (query_atomics ()) in
					let _ = userqs atomics atomics' functions in
					Bstats.time "leanr_from_samples" (learn_from_samples atomics pos_samples) neg_samples
				else (eager_termination := true; Bstats.time "gen_inv" (gen_inv (Hashtbl.create 0) pos_samples) neg_samples) in	
			let qualifiers = Bstats.time "remove_duplication" (Common.remove_customized_duplicates (fun q1 q2 -> match (q1, q2) with
				| ((Some k1, ((_, valu1, p1) as q1')), (Some k2, ((_, valu2, p2) as q2'))) -> 
					let b1 = List.exists (fun horef -> Path.same horef k1) horefs in
					let b2 = List.exists (fun horef -> Path.same horef k2) horefs in
					(b1 = b2 && Qualifier.equals q1' q2')
				| ((None, ((_, valu1, p1) as q1')), (None, ((_, valu2, p2) as q2'))) -> 
					Qualifier.equals q1' q2'
				| _ -> false
			)) (qualifiers) in
			(*let qualifiers = [List.nth qualifiers 0; List.nth qualifiers 1; List.hd (List.rev qualifiers)] in*)
			let _ = Format.fprintf Format.std_formatter "Number of qualifiers after remove duplication %d@." (List.length qualifiers) in
			(*let _ = List.iter (fun (_, (path, valu, pred)) ->
				fprintf std_formatter "Function %s with qualifier %a @." (Path.name path)
				Predicate.pprint' pred
			) qualifiers in*)
			if (Hashtbl.length invariants = 0 && (!array_inv_tried)) then (unsat, prev_s, prev_failed_invariant)
			else if (Hashtbl.length invariants = 0 && (not (!array_inv_tried))) then
				(array_inv_tried := true; (Clflags.gen_inv := false);
				loop pos_samples (Hashtbl.create 0) (Hashtbl.create 0) qualifiers unsat prev_s)
			else 
				(*let _ = assert (not (!array_inv_tried)) in*)
				let clean_s = Sol.copy s in
				let s = Bstats.time "make_further_solution" 
								(make_further_solution cs sri spec (qualifiers) clean_s horefs horeturnrefs unknows) unknowreturns in
				let _ = dump_solving qualifiers sri s 0  in 
			  let _ = Bstats.time "solving wfs" (solve_wf is_higher_order sri) s in
			  let _ = dump_solving qualifiers sri s 1 in
			  let w = make_initial_worklist sri in
				(** Important: Register local type *)
				let _ = Frame.register_unfound (local_types is_higher_order sri (solution_map s)) in
			  let _ = Bstats.time "solving sub" (solve_sub is_higher_order sri s) w in
			  let _ = dump_solving qualifiers sri s 2 in
			  let _ = TP.reset () in
			  let unsat = Bstats.time "testing solution" (unsat_constraints is_higher_order sri) s in
				if (List.length unsat > 0) then 
					(* Fixedme. Required a failed subset from the invariants *)
					if (!array_inv_tried) then 
						loop pos_samples neg_samples invariants qualifiers unsat s
					else 
						(array_inv_tried := true; (Clflags.gen_inv := false);
						loop pos_samples (Hashtbl.create 0) (Hashtbl.create 0) qualifiers unsat s)
				else (unsat, s, invariants) in
	(* An outer loop that iteratively tests the program for more postive samples *)	
	let testcounter = ref 0 in		
	let rec testloop unsat s invariants = 			
		if (List.length unsat > 0 || (!Clflags.gen_inv) || !testcounter = 0) (*&& (!testcounter < 2)*) then 
			let _ = testcounter := (!testcounter + 1) in
			let pos_samples = Bstats.time "query_pos_samples" query_pos_samples invariants in
			if (Hashtbl.length pos_samples = 0) then (unsat, s)
			else (* Proceeds with more samples *)
				let (unsat, s, invariants) = 
					loop pos_samples (Hashtbl.create 0) (Hashtbl.create 0) [] unsat s in
				let _ = if not !(Backwalker.hoflag) then (iter_count := 1) in
				(*if !(Backwalker.hoflag) (* if hoflag is set, iteratively generate more positive samples *)
				then*) testloop unsat s invariants (*else (unsat, s)*)
		else (unsat, s) in
	let (unsat, s) = testloop unsat s (Hashtbl.create 0) (*
		if (List.length unsat > 0 || (!Clflags.gen_inv)) then 
			let pos_samples = Bstats.time "query_pos_samples" query_pos_samples () in
			loop pos_samples (Hashtbl.create 0) (Hashtbl.create 0) [] unsat s
		else (unsat, s) 
	*) in
  (*(if List.length unsat > 0 then 
    C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
    List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat);*)
  (solution_map s, (List.map (fun (a, b) -> b)  unsat), !iter_count)