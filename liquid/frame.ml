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

open Parsetree (* must be opened before typedtree *)
open Types
open Typedtree
open Btype
open Format
open Asttypes

module C = Common
module PM = C.PathMap
module T = Typetexp

let returnrepr = "r"
let returnpath = Path.mk_ident returnrepr

(** Fix a set for higher order function parameters!! *)
let hoparams = ref [] 
let get_ho_param i = 
	let len = List.length (!hoparams) in
	if (i >= len) then
		(for j = len to i do 
			(hoparams := (!hoparams)@[Path.mk_ident ("ho_"^(string_of_int j))])
			done;
		List.nth (!hoparams) i)
	else List.nth (!hoparams) i	
	
let get_all_ho_params () = (!hoparams)

(** We also have a fixed set of existential variables *)		
let exparams = ref []
let get_ex_param i = 
	let len = List.length (!exparams) in
	if (i >= len) then
		(for j = len to i do 
			(exparams := (!exparams)@[Path.mk_ident ("ex_"^(string_of_int j))])
			done;
		List.nth (!exparams) i)
	else List.nth (!exparams) i	
	
let get_all_ex_params () = (!exparams)

(** We also maintain the flag of old array *)
let old_array_flag = "old_"

let old_array_fun = "old"

type substitution = Path.t * Predicate.pexpr

type open_assignment = Top | Bottom

type qualifier_expr =
    Qvar of (Path.t * open_assignment)  (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

(*type resource = Path.t * qualifier_expr
type effect = resource list*)

type t =
    Fvar of Path.t
  | Fconstr of Path.t * t list * variance list * refinement * effect
  | Farrow of pattern_desc option * t * t * effect
  | Ftuple of t list * refinement
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown
and effect = (Path.t * t) list
and variance = Covariant | Contravariant | Invariant

let empty_refinement = ([], Qconst [])

let false_refinement = ([], Qconst [(Path.mk_ident "false", Path.mk_ident "V", Predicate.Not (Predicate.True))])

let true_refinement = ([], Qconst [(Path.mk_ident "false", Path.mk_ident "V", (Predicate.True))])

let rec set_refinement fr ref = match fr with
	| Fvar _ -> fr
	| Fconstr (p, ts, vs, _, eff) -> Fconstr (p, ts, vs, ref, eff)
	| Farrow (p, f1, f2, eff) -> Farrow (p, f1, set_refinement f2 ref, eff)
	| Ftuple (ts, _) -> Ftuple (ts, ref)
	| Frecord (p, ts, _) -> Frecord (p, ts, ref)
	| Funknown -> fr

let update_refinement fr q = match fr with
	| Fvar _ -> fr
	| Fconstr (p, ts, vs, oldref, eff) -> 
		let ref = (match oldref with
			| (subs, Qvar _) -> oldref
			| (subs, Qconst qs) -> (subs, Qconst (q::qs))
		) in
		Fconstr (p, ts, vs, ref, eff)
	| Farrow _ -> fr
	| Ftuple (ts, oldref) -> 
		let ref = (match oldref with
			| (subs, Qvar _) -> oldref
			| (subs, Qconst qs) -> (subs, Qconst (q::qs))
		) in
		Ftuple (ts, ref)
	| Frecord (p, ts, oldref) -> 
		let ref = (match oldref with
			| (subs, Qvar _) -> oldref
			| (subs, Qconst qs) -> (subs, Qconst (q::qs))
		) in
		Frecord (p, ts, ref)
	| Funknown -> fr

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Path.name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let unique_name = (* Path.name *) Path.unique_name 

let pprint_refinement ppf refi =
  match refi with
    | (_, Qvar (id, _) ) ->
      fprintf ppf "%s" (unique_name id)
    | (subs, Qconst []) ->
      fprintf ppf "true"
    | (subs, Qconst quals) ->
      let preds = List.map (Qualifier.apply (Predicate.Var (Path.mk_ident "V"))) quals in
      let preds = List.map (Predicate.apply_substs subs) preds in
        Oprint.print_list Predicate.pprint (fun ppf -> fprintf ppf "@ ") ppf preds

let rec pprint_pattern ppf = function
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var x -> fprintf ppf "%s" (Ident.name x)
  | Tpat_tuple pats ->
      fprintf ppf "(%a)" pprint_pattern_list pats
  | Tpat_construct (cstrdesc, pats) ->
      begin match (repr cstrdesc.cstr_res).desc with
        | Tconstr (p, _, _) -> fprintf ppf "%s(%a)" (Path.name p) pprint_pattern_list pats
        | _ -> assert false
      end
  | _ -> assert false

and pprint_pattern_list ppf pats =
  Oprint.print_list pprint_pattern (fun ppf -> fprintf ppf ", ") ppf (List.map (fun p -> p.pat_desc) pats)

let refinement_is_empty = function
  | (_, Qconst []) -> true
  | _ -> false

let wrap_refined ppf pp = function
  | (_, Qconst []) -> pp ppf
  | r -> fprintf ppf "@[{"; pp ppf; fprintf ppf " |@;<1 2>%a}" pprint_refinement r

let rec pprint ppf = function
  | Fvar a ->
      fprintf ppf "Var(%s)" (unique_name a)
  | Fconstr (path, [], _, r, effect) ->
      (wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () path)) r;
			fprintf ppf "/[%a]" pprint_effect effect)
  | Farrow (None, f, f', effect) ->
      fprintf ppf "@[%a@ ->@;<1 2>%a / [%a]@]" pprint1 f pprint f' pprint_effect effect
  | Farrow (Some pat, f, f', effect) ->
      fprintf ppf "@[%a:@ %a@ ->@;<1 2>%a / [%a]@]" pprint_pattern pat pprint1 f pprint f'
			pprint_effect effect
  | Fconstr (path, l, _, r, effect) ->
      (wrap_refined ppf (fun ppf -> fprintf ppf "%a@ %s" (pprint_list ",") l (C.path_name () path)) r;
			fprintf ppf "/[%a]" pprint_effect effect)
  | Ftuple (ts, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "(%a)" (pprint_list "*") ts) r
  | Frecord (id, _, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () id)) r
  | Funknown ->
      fprintf ppf "[unknown]"
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f
 and pprint_list sep ppf = Oprint.print_list pprint (fun ppf -> fprintf ppf "@;<1 2>%s@;<1 2>" sep) ppf
 and pprint_effect ppf effect = 
	List.iter (fun (p, f) -> 
		fprintf ppf "%s:%a;" (Path.name p) pprint f) effect 

let rec pprint_fenv ppf fenv =
  ignore(
		Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (Path.unique_name k) pprint v) fenv)

let translate_variance = function
  | (true, true, true) -> Invariant
  | (true, false, false) -> Covariant
  | (false, true, false) -> Contravariant
  | _ -> assert false

let rec same_shape map_vars t1 t2 =
  let vars = ref [] in
  let ismapped p q = try snd (List.find (fun (p', q) -> Path.same p p') !vars) = q with
      Not_found -> vars := (p, q) :: !vars; true in
  match (t1, t2) with
  (Fconstr(p, l, _, _, _), Fconstr(p', l', _, _, _)) ->
   (Path.same p p') && (List.for_all (fun f -> f) (List.map2 (same_shape map_vars) l l')) 
  | (Fvar p, Fvar p') ->
   if map_vars then ismapped p p' else Path.same p p'
  | (Farrow(_, i, o, _), Farrow(_, i', o', _)) ->
   ((same_shape map_vars) i i') && ((same_shape map_vars) o o')
  | (Ftuple (t1s, _), Ftuple (t2s, _)) ->
   List.for_all2 (same_shape map_vars) t1s t2s
  | (Frecord (p1, f1s, _), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
      let shape_rec (f1, _, _) (f2, _, _) = (same_shape map_vars) f1 f2 in
        List.for_all2 shape_rec f1s f2s
  | (Funknown, Funknown) -> true
  | t -> false 




let rec map f = function
    | (Funknown | Fvar _) as fr -> f fr
    | Fconstr (p, fs, cstrdesc, r, effect) -> 
			f (Fconstr (p, List.map (map f) fs, cstrdesc, r, List.map (fun (p, fr) -> (p, map f fr)) effect))
    | Ftuple (fs, r) -> f (Ftuple (List.map (map f) fs, r))
    | Farrow (x, f1, f2, effect) -> 
			f (Farrow (x, map f f1, map f f2, List.map (fun (p, fr) -> (p, map f fr)) effect))
    | Frecord (p, fs, r) -> f (Frecord (p, List.map (fun (fr, n, m) -> (map f fr, n, m)) fs, r))

let rec map_refinements_map f = function
  | Fconstr (p, fs, cstrdesc, r, effect) -> 
		Fconstr (p, fs, cstrdesc, f r, effect)
  | Ftuple (fs, r) -> Ftuple (fs, f r)
  | Frecord (p, fs, r) -> Frecord (p, fs, f r)
  | f -> f

let map_refinements f fr = map (map_refinements_map f) fr

let rec refresh_refinement fr = 
	map_refinements (fun (subs, qe) -> 
	match qe with 
		| Qvar (k, oa) -> ([], Qvar (Path.mk_ident "k", oa)) (* Qualifier variable *)
  	| Qconst _ -> ([], Qvar (Path.mk_ident "k", Top))
	) fr
	

(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate fr ftemplate =
  let vars = ref [] in
  let rec inst f ft =
		(*let inst_effect effect effect' = 
			try (List.fold_left (fun res (p, fr) -> 
				let (_, fr') = List.find (fun (p', fr') -> Path.same p p') effect' in
				res @ [(p, inst fr fr')]
			) [] effect) with _ -> assert false in*)
    match (f, ft) with
      | (Fvar _, _) -> (try List.assq f !vars with Not_found -> vars := (f, ft) :: !vars; ft)
      | (Farrow (l, f1, f1', effect), Farrow (_, f2, f2', effect')) ->
				Farrow (l, inst f1 f2, inst f1' f2', effect)
      | (Fconstr (p, l, varis, r, effect), Fconstr(p', l', _, _, effect')) ->
				Fconstr(p, List.map2 inst l l', varis, r, effect)
      | (Ftuple (t1s, r), Ftuple (t2s, _)) ->
         Ftuple (List.map2 inst t1s t2s, r)
      | (Frecord (p, f1s, r), Frecord (_, f2s, _)) ->
         let inst_rec (f1, name, m) (f2, _, _) = (inst f1 f2, name, m) in
         Frecord (p, List.map2 inst_rec f1s f2s, r)
      | (Funknown, Funknown) -> Funknown
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false in
  let fr = inst fr ftemplate in
	let rec inst_effect fr = match fr with
		| Fvar _ -> (
			try let fr = List.assq fr !vars in fr with _ -> fr)
		| Farrow (l, f1, f1', effect) -> 
			Farrow (l, inst_effect f1, inst_effect f1', 
				List.map (fun (p, fr) -> (p, inst_effect fr)) effect)
		| Fconstr (p, fs, varis, r, effect) -> 
			Fconstr (p, List.map (fun fr -> inst_effect fr) fs, varis, r, 
				List.map (fun (p, fr) -> (p, inst_effect fr)) effect)
		| Ftuple (fs, r) -> Ftuple (List.map (fun fr -> inst_effect fr) fs, r)
		| Frecord (p, fs, r) -> 
			Frecord (p, List.map (fun (fr, n, m) -> (inst_effect fr, n, m)) fs, r)
		| Funknown -> Funknown in
	inst_effect fr

let instantiate_qualifiers_map vars = function
  | (subs, Qconst qs) ->
      (subs, Qconst (List.map (fun q -> match Qualifier.instantiate vars q with Some q -> q | None -> q) qs))
  | r -> r

let instantiate_qualifiers vars fr =
  map_refinements (instantiate_qualifiers_map vars) fr

let fresh_refinementvar open_assn () = ([], Qvar (Path.mk_ident "k", open_assn))
let fresh_true () = ([], Qconst ([(C.dummy (), Path.mk_ident "true", Predicate.True)]))

let fresh_fvar () = Fvar (Path.mk_ident "a")

(* 1. Tedium ahead:
   OCaml stores information about record types in two places:
    - The type declaration stores everything that's set in stone about the
     type: what its fields are and which variables are the type parameters.
    - The type list of the Tconstr contains the actual instantiations of
     the tyvars in this instantiation of the record. *)

let fresh_constr freshf constrf p ty_decl f g tyl fresh =
  match ty_decl.type_kind with
  | Type_abstract | Type_variant _ ->
    if Path.same p Predef.path_unit then 
      Fconstr(p, [], [], ([], Qconst []), []) 
    else
      Fconstr(p, f tyl, List.map translate_variance ty_decl.type_variance, constrf (), [])
  | Type_record (fields, _) -> (* 1 *)
    let param_map = List.combine ty_decl.type_params tyl in
    let fresh_field (name, muta, typ) =
      let field_typ = try g (List.assoc typ param_map) with 
        Not_found -> fresh freshf typ in
        (field_typ, name, muta) in
    Frecord (p, List.map fresh_field fields, constrf ())

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun vars env ty fresh_ref_var =
  let rec fresh_rec freshf t =
    let t = repr t in
    match t.desc with
        Tvar ->
          (try List.assq t !vars with Not_found ->
            let fv = fresh_fvar () in 
            vars := (t, fv) :: !vars; fv)
      | Tconstr(p, tyl, _) ->
          let ty_decl = Env.find_type p env in
            fresh_constr freshf freshf p ty_decl (List.map (fresh_rec freshf)) (fresh_rec freshf) tyl fresh_rec
      | Tarrow(_, t1, t2, _) -> Farrow (None, fresh_rec freshf t1, fresh_rec freshf t2, [])
      | Ttuple ts -> Ftuple (List.map (fresh_rec freshf) ts, freshf ())
      | _ -> fprintf err_formatter "@[Warning: Freshing unsupported type]@."; Funknown
  in fresh_rec fresh_ref_var (repr ty)

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations.

   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh env ty = fresh_with_var_fun (ref []) env ty (fresh_refinementvar Top)
let transl_ty env ty = fresh_with_var_fun (ref []) env ty fresh_true

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars env ty = fresh_with_var_fun (ref []) env ty (fun _ -> empty_refinement)

let fresh_unconstrained env ty = fresh_with_var_fun (ref []) env ty (fresh_refinementvar Bottom)

let fresh_constructor env cstrdesc = function
  | Fconstr (_, fl, _, _, _) ->
      let tyargs = match cstrdesc.cstr_res.desc with Tconstr(_, args, _) -> args | _ -> assert false in
      let argmap = ref (List.combine (List.map repr tyargs) fl) in
        List.map (fun t -> fresh_with_var_fun argmap env t (fresh_refinementvar Top)) cstrdesc.cstr_args
  | _ -> assert false

let transl_pref plist env p = 
  let fp s = 
    let b = try List.find (fun (nm, _) -> nm = s) plist with
      Not_found -> failwith (Printf.sprintf "%s not found in mlq\n" s) in
    (fun (_, p) -> p) b in
  let (v, p) =
    match p with
    | RLiteral (v, p) -> (v, p)
    | RVar s -> fp s in
  let valu = Path.mk_ident v  in
  ([], Qconst([(C.dummy (), valu, Qualdecl.transl_patpred_single false valu env p)]))

let rec translate_pframe env plist pf =
  let vars = ref [] in
  let getvar a = try List.find (fun b -> Path.name b = a) !vars
                   with Not_found -> let a = Path.mk_ident a in
                   let _ = vars := a::!vars in
                     a in
  let transl_pref = transl_pref plist env in
  let rec transl_pframe_rec pf =
    match pf with
    | PFvar (a, r) -> Fvar (getvar a)
    | PFconstr (l, fs, r) -> transl_constr l fs r
    | PFarrow (v, a, b) ->
        let pat = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem (Path.Pident id) !vars then failwith "Redefined variable";
                vars := Path.Pident id :: !vars; Some (Tpat_var id)
          | None -> None
        in Farrow (pat, transl_pframe_rec a, transl_pframe_rec b, [])
    | PFtuple (fs, r) -> Ftuple (List.map transl_pframe_rec fs, transl_pref r)
    | PFrecord (fs, r) -> transl_record fs r 
  and transl_constr l fs r =
    let (path, decl) = try Env.lookup_type l env with
      Not_found -> raise (T.Error(Location.none, T.Unbound_type_constructor l)) in
    let _ = if List.length fs != decl.type_arity then
      raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
    let fs = List.map transl_pframe_rec fs in
    let fresh freshf ty = fresh_with_var_fun (ref []) env ty freshf in
    let id = (fun f -> f) in
    let refinement () = transl_pref r in
      fresh_constr fresh_true refinement path decl id id fs fresh
  and transl_record fs r =
    let fs = List.map (fun (f, s, m) -> (transl_pframe_rec f, s, m)) fs in
    let path = Path.mk_ident "_anon_record" in
    Frecord(path, fs, transl_pref r) in
  transl_pframe_rec pf

let bind env pat frame =
  let _bind = function
    | (Tpat_any, _) -> ([], [])
    | (Tpat_var x, f) -> ([], [(Path.Pident x, f)])
    | (Tpat_tuple pats, Ftuple (fs, _)) ->
        (List.combine (Pattern.pattern_descs pats) fs, [])
    | (Tpat_construct (cstrdesc, pats), f) ->
        (List.combine (Pattern.pattern_descs pats) (fresh_constructor env cstrdesc f), [])
    | _ -> assert false
  in C.expand _bind [(pat, frame)] []

let env_bind tenv env pat frame =
  Lightenv.addn (bind tenv pat frame) env

(* pmr: this function got some pre-PLDI uglies that need to be ironed out -
   we need a partial instantiation method for qualifiers instead of accumulating these
   vars *)
(* Label all the function formals in [f] with their corresponding labels in
   [f'] and changing constant qualifiers appropriately.
   [f] and [f'] are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let label_like f f' =
  let rec label vars f f' = 
		let label_effect vars effect effect' = 
			try List.fold_left (fun res (p, fr) -> 
				let (p', fr') = List.find (fun (p', fr') -> Path.same p p') effect'	in
				res @ [(p, label vars fr fr')]
			) [] effect with _ -> assert false in
		match (f, f') with
    | (Fvar _, Fvar _) | (Funknown, Funknown) | (Fconstr _, Fconstr _) -> instantiate_qualifiers vars f
    | (Farrow (None, f1, f1', effect), Farrow (l, f2, f2', effect')) ->
      Farrow (l, label vars f1 f2, label vars f1' f2', label_effect vars effect effect')
    | (Farrow (Some p1, f1, f1', effect), Farrow (Some p2, f2, f2', effect')) ->
			let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
    	Farrow (Some p2, label vars f1 f2, label vars' f1' f2', label_effect vars' effect effect')
    | (Ftuple (t1s, r), Ftuple (t2s, _)) ->
      Ftuple (List.map2 (label vars) t1s t2s, r)
    | (Frecord (p1, f1s, r), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
      let label_rec (f1, n, muta) (f2, _, _) = (label vars f1 f2, n, muta) in
      Frecord (p1, List.map2 label_rec f1s f2s, r)
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  in label [] f f'

(* Create a fresh frame with the same shape as [exp]'s type and [f],
   and the same labels as [f]. *)
let fresh_with_labels env ty f = label_like (fresh env ty) f

(* The idea: sub should be inserted from the back end instead of the front end
	 new sub should be consistent with old subs	*)
let insert_sub sub subs = (
	let sub = List.fold_left (fun (p, pe) (p', pe') -> 
		let hops = get_all_ho_params () in
		if (Path.same p p' && List.for_all (fun hop -> not (Path.same p hop)) hops) then
			match pe' with
				| (Predicate.Var pe') -> 
					(assert (List.exists (fun hop -> Path.same pe' hop) hops); (pe', pe))
				| _ -> assert false 
		else (p, pe)
	) sub subs in
	subs@[sub]	
)

let apply_substitution_map sub = function
  | Fconstr (p, fs, cstrs, (subs, qe), effect) -> 
		Fconstr (p, fs, cstrs, ((*sub :: subs*)insert_sub sub subs, qe), 
		List.map (fun (p, fr) -> match sub with
			(path, pexpr) -> 
				(if (Path.same path p) then 
				match pexpr with
					| Predicate.Var path' -> (path', fr)	
					| _ -> assert false
				else (p, fr))
		) effect)
  | Frecord (p, fs, (subs, qe)) -> 
		Frecord (p, fs, ((*sub :: subs*)insert_sub sub subs, qe))
	| Ftuple (fs, (subs, qe)) ->
		Ftuple (fs, (insert_sub sub subs, qe))
  | f -> f

let apply_substitution sub f = map (apply_substitution_map sub) f

let unfound = ref (fun _ -> assert false) 

let register_unfound f = (unfound := f)

let refinement_apply_solution solution = function
  | (subs, Qvar (k, _)) -> 
		(subs, Qconst (try solution k with Not_found -> 
			(Format.fprintf Format.std_formatter "Solution: Keep looking %s@." (Path.unique_name k); (*(!unfound) k*)assert false)))
  | r -> r

let apply_solution_map solution = function
  | Fconstr (path, fl, cstrs, r, effect) -> 
		Fconstr (path, fl, cstrs, refinement_apply_solution solution r, effect)
  | Frecord (path, fs, r) -> Frecord (path, fs, refinement_apply_solution solution r)
  | Ftuple (fs, r) -> Ftuple (fs, refinement_apply_solution solution r)
  | f -> f

let apply_solution solution fr = map (apply_solution_map solution) fr

let ref_var = function
  | (_, Qvar (k, _)) -> Some k
  | _ -> None

(** NO Refinement vars in effect part should be countered *)
let rec refinement_vars = function
  | Fconstr (_, _, _, r, _) -> C.maybe_cons (ref_var r) []
  | Frecord (_, fs, r) ->
      C.maybe_cons (ref_var r) (List.fold_left (fun r (f, _, _) -> refinement_vars f @ r) [] fs)
  | Ftuple (fs, r) ->
      C.maybe_cons (ref_var r) (List.fold_left (fun r f -> refinement_vars f @ r) [] fs)
  | _ -> []


let rec all_refinement_vars = function
  | Fconstr (_, fs, _, r, effect) -> 
			C.maybe_cons (ref_var r) ((List.fold_left (fun res (_, f) -> res @ (all_refinement_vars f)) [] effect) @
			(List.fold_left (fun rest f -> all_refinement_vars f @ rest) [] fs))
  | Frecord (_, fs, r) ->
      C.maybe_cons (ref_var r) (List.fold_left (fun r (f, _, _) -> all_refinement_vars f @ r) [] fs)
  | Ftuple (fs, r) ->
      C.maybe_cons (ref_var r) (List.fold_left (fun r f -> all_refinement_vars f @ r) [] fs)
	| Farrow (_, f1, f2, effect) -> 
		(all_refinement_vars f1) @ (all_refinement_vars f2) @ 
		(List.fold_left (fun res (_, f) -> res @ (all_refinement_vars f)) [] effect)
  | _ -> []

let apply_refinement r = function
  | Fconstr (p, fl, varis, _, effect) -> Fconstr (p, fl, varis, r, effect)
  | Frecord (p, fs, _) -> Frecord (p, fs, r)
  | Ftuple (fs, _) -> Ftuple (fs, r)
  | f -> f

let refinement_conjuncts cache solution qual_expr (subs, qualifiers) =
  let quals = match qualifiers with
    | Qvar (k, _) -> (try solution k with Not_found -> 
			(
			let result = 
			(!unfound) cache qual_expr k in result))
    | Qconst qs -> qs
  in
  let unsubst = List.map (Qualifier.apply qual_expr) quals in
    List.map (Predicate.apply_substs subs) unsubst

let refinement_predicate cache solution qual_var refn =
  Predicate.big_and (refinement_conjuncts cache solution qual_var refn)

(** NO Refinement vars in effect part should be countered *)
let rec conjunct_fold cache cs solution qual_expr = function
  | Fconstr(_, _, _, r, _) -> refinement_conjuncts cache solution qual_expr r @ cs
  | Frecord (p, fs, r) ->
      let subframe_fold b (f, name, _) =
        conjunct_fold cache b solution (Predicate.Field (name, qual_expr)) f
      in refinement_conjuncts cache solution qual_expr r @ List.fold_left subframe_fold cs fs
  | Ftuple (fs, r) ->
      let subframe_fold i b f =
        conjunct_fold cache b solution (Predicate.Proj (i, qual_expr)) f
      in refinement_conjuncts cache solution qual_expr r @ C.fold_lefti subframe_fold cs fs
	(*| Farrow _ as f ->
		let rec loop index f cs = match f with
			| Farrow (_, f1', f2') -> 
				loop (index+1) f2' cs
					(*(conjunct_fold cs solution 
					(Predicate.FunApp ("Arg2", 
					[qual_expr; Predicate.Var (get_ho_param index)])) f1')*)
			| _ -> 
				conjunct_fold cs solution 
				(Predicate.FunApp ("Ret"^(string_of_int (1+index)), 
					qual_expr::(Array.to_list (Array.init index 
					(fun i -> 
						(Predicate.FunApp ("Arg2",
						[qual_expr; Predicate.Var (get_ho_param i)]))
						))))) f in
		loop 0 f cs*)
  | _ -> cs

let rec conjuncts cache solution qual_expr fr =
  conjunct_fold cache [] solution qual_expr fr

let predicate cache solution qual_expr f =
  Predicate.big_and (conjuncts cache solution qual_expr f)
	
(* Fixme? A simple function of extracting refinement value from frame *)
let rec get_refinement_variable fr = match fr with
	| Fconstr (_,_,_,(_,Qvar (k, _)),_) -> Some k
	| Farrow (_,_,f2,_) -> get_refinement_variable f2
	| Frecord (_,_,(_,Qvar (k, _))) -> Some k
	| Ftuple (_,(_,Qvar (k, _))) -> Some k
	| _ -> None

(* Fixme. For the above two functions, shall we include effect in?? *)
let rec get_refinements fr = match fr with
	| Farrow (_,f1,f2,_) -> (get_refinements f1) @ (get_refinements f2)
	| Fconstr (_,_,_,(_,Qvar (k, _)),_) -> [(Some k)]
	| Frecord (_,fs,(_,Qvar (k, _))) -> 
		List.fold_left (fun res (f,_,_) -> res @ (get_refinements f)) [Some k] fs 
	| Ftuple (fs,(_,Qvar (k, _))) -> 
		List.fold_left (fun res f -> res @ (get_refinements f)) [Some k] fs 
	| _ -> [None]


let rec count_named_args fr = match fr with
	| Farrow (Some l, f1, f2, _) -> 1 + count_named_args f2
	| _ -> 0

let rec count_args fr = match fr with
	| Farrow (_, f1, f2, _) -> 1 + count_args f2
	| _ -> 0

let rec get_refinement_variable_by_index fr index = match fr with
	| Farrow (_, f1, f2, _) -> 
		if (index > 0) then get_refinement_variable_by_index f2 (index-1)
		else get_refinement_variable f1
	| _ -> assert false
		
let rec get_fun_bindings env fr = match fr with
	| Farrow (Some l, f1, f2, _) -> 
		let patfrs = bind env l f1 in
		patfrs @ (get_fun_bindings env f2)
	| _ -> [(returnpath, fr)]
	
let rec push_effect_fst effect fr = match fr with	
	| Farrow (Some x, f1 ,f2, efs) ->
		(assert (List.length efs = 0);
		Farrow (Some x, f1, push_effect_fst effect f2, efs))
	| Farrow (None, f1, f2, efs) -> 
		(assert (List.length efs = 0);
		Farrow (None, f1, f2, effect))
	| Fconstr (p, fs, cstrdesc, r, efs) -> (
		assert (List.length efs = 0); 
		Fconstr (p, fs, cstrdesc, r, effect)) 	
	| Fvar _ -> (assert (List.length effect = 0); fr)
	| Frecord _ -> (assert (List.length effect = 0); fr)   
	| Ftuple _ -> (assert (List.length effect = 0); fr)  
	| _ -> (
		Format.fprintf Format.std_formatter "Fatal fr = %a and effect = %d@." pprint fr (List.length effect); 
		assert false)
	
	
(** Push the effects into the function result for a higher order function *)
let rec push_effect_ho effect fr = match fr with
		| Farrow (None, f1, f2, efs) ->
			(assert (List.length efs = 0);
			Farrow (None, f1, push_effect_ho effect f2, efs))
		| Fconstr (p, fs, cstrdesc, r, efs) -> (
			assert (List.length efs = 0); 
			Fconstr (p, fs, cstrdesc, r, effect)) 
		| _ -> assert false	

let push_effect env context fr effobjs = 
	(* the effect object must be included in the environment or be
  	 bounded by lambda expression *)
	let candidates = Lightenv.maplistfilter (fun v f -> match f with  
		| Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
			if (List.exists (fun obj -> Path.same obj v) effobjs) 
			then Some (v, f) else None
		| _ -> None
	) context in 
	let bounds = get_fun_bindings env fr in
	let bounds = Common.map_partial (fun (p, f) -> match f with
		| Fconstr (x,_,_,_,_) when x = Predef.path_array ->	
			if (Path.same returnpath p) then None
			else if (List.exists (fun obj -> Path.same obj p) effobjs)
			then Some (p, f) else None
		| _ -> None
	) bounds in
	let effect = 
		List.map (fun (v, f) -> (v, refresh_refinement f)) (candidates@bounds) in	
	push_effect_fst effect fr
	
	
(** Infer the template of effects for dependent types *)
let fresh_effect env context fr = 
	let rec collect_effects hoindex res fr = match fr with
		| Farrow (None, f1, f2, []) -> collect_effects (hoindex+1) (
			let p = get_ho_param hoindex in
			let pid = match p with Path.Pident id -> id | _ -> assert false in
			let pfrs = bind env (Tpat_var pid) f1 in
			res @ (Common.map_partial (fun (p, f) -> match f with
				| Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
					Some (p, refresh_refinement f)
				| _ -> None
			) pfrs)
		) f2
		| _ -> res in
	let rec fresh fr = match fr with
		| Farrow (Some _, f1, f2, _) -> 
			(* the effect must include candidates from both environment variable and 
		  	variables bounded by lambda expression *)
			let candidates = Lightenv.maplistfilter (fun v f -> match f with  
				| Fconstr (x,_,_,_,_) when x = Predef.path_array -> Some (v, f)
				| _ -> None
			) context in 
			let effect = List.map (fun (v, f) -> (v, refresh_refinement f)) candidates in	
			let bounds = get_fun_bindings env fr in
			let bounds = Common.map_partial (fun (p, f) -> match f with
				| Fconstr (x,_,_,_,_) when x = Predef.path_array ->	
					if (Path.same returnpath p) then None
					else Some (p, refresh_refinement f)
				| _ -> None
			) bounds in
			let effect = effect @ (List.map (fun (v, f) -> (v, refresh_refinement f)) bounds) in
			push_effect_fst effect fr
		| Farrow (None, f1, f2, _) ->
			let effect = collect_effects 0 [] fr in
			push_effect_ho effect fr
    | Fconstr (p, fs, cstrdesc, r, _) -> 
			let candidates = Lightenv.maplistfilter (fun v f -> match f with  
				| Fconstr (x,_,_,_,_) when x = Predef.path_array -> Some (v, f)
				| _ -> None
			) context in 
			let effect = List.map (fun (v, f) -> (v, refresh_refinement f)) candidates in	
			push_effect_fst effect fr
    | Ftuple (fs, r) -> Ftuple (List.map (fresh) fs, r)
    | Frecord (p, fs, r) -> Frecord (p, List.map (fun (fr, n, m) -> (fresh fr, n, m)) fs, r)
		| (Funknown | Fvar _) as fr -> 
			let candidates = Lightenv.maplistfilter (fun v f -> match f with  
				| Fconstr (x,_,_,_,_) when x = Predef.path_array -> Some (v, f)
				| _ -> None
			) context in 
			(assert (List.length candidates = 0); fr) in
	let fr = fresh fr in
	let _ = Format.fprintf Format.std_formatter "freshed frame:%a@." pprint fr in
	fr

let rec eff fr = match fr with
	| Farrow (_, f1, f2, effect) -> effect
	| Fconstr (p, fs, cstrdesc, r, effect) -> effect
	| Ftuple (fs ,r) -> List.fold_left (fun res f -> res @ (eff f)) [] fs
	| Frecord (p, fs, r) -> List.fold_left (fun res (f, _, _) -> res @ (eff f)) [] fs 
	| (Funknown | Fvar _) -> []

let rec ty fr = match fr with
	| Farrow (p, f1, f2, effect) -> Farrow (p, f1 ,f2, [])
	| Fconstr (p, fs, cstrdesc, r, effect) -> Fconstr (p, fs, cstrdesc, r, [])
	| Ftuple (fs, r) -> Ftuple (List.map (ty) fs, r)
	| Frecord (p,fs, r) -> Frecord (p, List.map (fun (fr, n, m) -> (ty fr, n, m)) fs, r)
	| (Funknown | Fvar _) as fr -> fr

let update_arrayencoding p refresh_id env = 
	let (arraygets, env) = Lightenv.partition (fun p _ -> 
		let p = Path.name p in
		(String.compare p ("arraygetencoding") = 0 || 
			String.compare p ("fixarrayencoding") = 0 )
		) env in
	let arraygets = Lightenv.map (fun fr -> match fr with
		| Fconstr (_,_,_,([], Qconst qs),_) when List.length qs = 1 ->
			let (a,b,pred) = List.hd qs in
			let pred = Predicate.subst (Predicate.Var refresh_id) p pred in
			Fconstr(Predef.path_int, [], [], ([], Qconst [(a,b,pred)]), [])
		| _ -> assert false) arraygets in
	let arraygets = Lightenv.bindings arraygets in
	Lightenv.addn arraygets env 

let refresh_flag = Path.mk_ident "envrefreshed"		
	
(** Make a flag in the environment to indicate that this env has been refreshed due to effect *)	
let make_env_refreshed env = 
	Lightenv.add refresh_flag (Fconstr(Predef.path_int, [], [], ([], Qconst []), [])) env

(** If now_effect is found containing an array update then merge it with environment 
		Also return the effect id (p field) so the farther expression also generate the effect on p*)		
let update_env env effect = 
	let env = if (List.length effect > 0) then make_env_refreshed env else env in
	List.fold_left (fun env (p, fr) -> match fr with
		| Fconstr (x,y,z,r,effect) when x = Predef.path_array -> (
			(** The original array in the env is renamed
					with this p  *)
			match r with
				| (subs, Qconst qs) -> 
					(*Strategy: 1. update in the subs the array id to a refresh array id
					   2. update in the env the array id to the same refreshed array id *)
					(assert (List.length qs > 0); 
					(** effect cannot be nested *)
					let _ = assert (List.length effect = 0) in
					let (o_fr, env) = (Lightenv.find p env, Lightenv.remove p env) in
					let refresh_id = Path.mk_ident (Path.name p) in
					let _ = Format.fprintf Format.std_formatter "old p = %s and new p = %s@." 
							(Path.unique_name p) (Path.unique_name refresh_id) in
					let subs = List.map (fun (param, pe) -> 
						(param, Predicate.exp_apply_substs [(p, Predicate.Var refresh_id)] pe)
					) subs in
					let fr = Fconstr (x,y,z,(subs, Qconst qs),effect) in
					let env = Lightenv.addn [(refresh_id, o_fr)] env in
					let env = Lightenv.addn [(p, fr)] env in
					(* 3. update all the arrayencodings *)
					update_arrayencoding p refresh_id env)
				| (subs, Qvar (k, os))  -> 
					if (List.length subs = 0) then Lightenv.addn [(p, fr)] env else 
					(** effect cannot be nested *)
					let _ = assert (List.length effect = 0) in
					(* Important: we deal with the case where a function is recursively called in one environment *)
					let smallenv = (Lightenv.filter (fun _ fr -> match fr with 
						| Fconstr (_,_,_,(_,(Qvar (k',_))),_) -> (Path.same k k')
						| _ -> false
					) env) in
					let _ = assert (Lightenv.cardinal smallenv <= 1) in
					let (subs, refresh_id, o_fr, env) =
						if (Lightenv.cardinal smallenv = 1) then
							(* The call should reuse the original refresh_id so we need to search for the refresh_id *)
							let refresh_id = Lightenv.fold (fun key fr res -> match fr with
								| Fconstr (_,_,_,(subs,(Qvar _)),_) -> 
									let pe = snd (List.find (fun (param, _) -> 
										String.compare (Path.name param) (old_array_flag^(Path.name p)) = 0
									) subs) in
									Predicate.exp_vars pe
								| fr -> assert false
							) smallenv [] in
							let _ = assert (List.length refresh_id = 1) in
							let refresh_id = List.hd refresh_id	in
							let (o_fr, env) = (Lightenv.find p env, Lightenv.remove p env) in
							let _ = Format.fprintf Format.std_formatter "old p = %s and new p = %s and k = %s@." 
							(Path.unique_name p) (Path.unique_name refresh_id) (Path.unique_name k) in
							let subs = subs @ [
								let (param, pe) = List.find (fun (param, pe) -> (pe = Predicate.Var p)) subs in
								(Path.Pident (Ident.create_with_stamp (old_array_flag ^ (Path.name param)) (Path.stamp param)), Predicate.Var refresh_id)
								] in
							let refresh_id = Path.mk_ident (Path.name p) in
							let env = update_arrayencoding p refresh_id env in
							(*make refresh_id = p*)
							let env = Lightenv.add (Path.mk_ident "fixarrayencoding")
								(Fconstr (Predef.path_int, [], [], ([], Qconst [(Path.mk_ident "", Path.mk_ident "", 
									Predicate.Atom (Predicate.Var p,Predicate.Eq,Predicate.Var refresh_id))]), []))
								env in
							(subs, refresh_id, o_fr, env)
						else 
							let refresh_id = Path.mk_ident (Path.name p) in
							let (o_fr, env) = (Lightenv.find p env, Lightenv.remove p env) in
							let _ = Format.fprintf Format.std_formatter "old p = %s and new p = %s and k = %s@." 
							(Path.unique_name p) (Path.unique_name refresh_id) (Path.unique_name k) in
							let subs = subs @ [
								let (param, pe) = List.find (fun (param, pe) -> (pe = Predicate.Var p)) subs in
								(Path.Pident (Ident.create_with_stamp (old_array_flag ^ (Path.name param)) (Path.stamp param)), Predicate.Var refresh_id)
								] in
							let env = update_arrayencoding p refresh_id env in
							(subs, refresh_id, o_fr, env) in
					let fr = Fconstr (x,y,z,(subs, Qvar (k, os)),effect) in
					(*let env = (** oldarray should now point to refresh_id *)
						Lightenv.add (Path.mk_ident "refresharrayencoding")
						(Fconstr (Predef.path_int, [], [], ([], Qconst [(Path.mk_ident "", Path.mk_ident "", 
							Predicate.Atom (Predicate.FunApp (old_array_fun, [Predicate.Var p]),
								Predicate.Eq,Predicate.Var refresh_id))]), [])) env in*)
					let env = Lightenv.addn [(refresh_id, o_fr)] env in
					let env = Lightenv.addn [(p, fr)] env in
					env
			)
		| _ -> Lightenv.addn [(p, fr)] env
	) env effect 
	
(*let update_env env effect = 
	List.fold_left (fun env (p, fr) -> match fr with
		| Fconstr (x,y,z,r,effect) when x = Predef.path_array -> (
			(** The original array in the env is renamed
					with this p  *)
			match r with
				| (subs, Qconst qs) -> 
					(*Strategy: 1. update in the subs the array id to a refresh array id
					   2. update in the env the array id to the same refreshed array id *)
					(assert (List.length qs > 0); 
					(** effect cannot be nested *)
					let _ = assert (List.length effect = 0) in
					let (o_fr, env) = (Lightenv.find p env, Lightenv.remove p env) in
					let refresh_id = Path.mk_ident (Path.name p) in
					let _ = Format.fprintf Format.std_formatter "old p = %s and new p = %s@." 
							(Path.unique_name p) (Path.unique_name refresh_id) in
					let subs = List.map (fun (param, pe) -> 
						(param, Predicate.exp_apply_substs [(p, Predicate.Var refresh_id)] pe)
					) subs in
					let fr = Fconstr (x,y,z,(subs, Qconst qs),effect) in
					let env = Lightenv.addn [(refresh_id, o_fr)] env in
					let env = Lightenv.addn [(p, fr)] env in
					(* 3. update all the arrayencodings *)
					update_arrayencoding p refresh_id env)
				| (subs, Qvar (k, os))  -> 
					(* strategy: if the environment contains another binding of the same k 
					   keep the original binding, rename it, make an exp rename=oldname *)
					let smallenv = (Lightenv.filter (fun key fr -> match fr with 
						| Fconstr (_,_,_,(_,(Qvar (k',_))),_) -> (Path.same p key) && (Path.same k k')
						| _ -> false
					) env) in
					let _ = assert (Lightenv.cardinal smallenv <= 1) in
					if (Lightenv.cardinal smallenv = 1) then (*follow our strategy*)
						let (o_fr, env) = (Lightenv.find p env, Lightenv.remove p env) in
						let refresh_id = Path.mk_ident (Path.name p) in
						let env = Lightenv.addn [(refresh_id, o_fr)] env in
						let env = Lightenv.addn [(p, fr)] env in
						let _ = Format.fprintf Format.std_formatter "old p = %s and new p = %s and k = %s@." 
							(Path.unique_name p) (Path.unique_name refresh_id) (Path.unique_name k) in
						(*make refresh_id = p*)
						Lightenv.add (Path.mk_ident "fixarrayencoding")
						(Fconstr (Predef.path_int, [], [], ([], Qconst [(Path.mk_ident "", Path.mk_ident "", 
							Predicate.Atom (Predicate.Var p,Predicate.Eq,Predicate.Var refresh_id))]), []))
						env
					else Lightenv.addn [(p, fr)] env
			)
		| _ -> Lightenv.addn [(p, fr)] env
	) env effect*)
	
let bindpat pat_var = 
	match pat_var with
    | (Tpat_any, _) -> ([], [])
    | (Tpat_var x, var) -> ([], [(Path.Pident x, var)])
    | (Tpat_tuple pats, var) ->
        (List.combine (Pattern.pattern_descs pats) (Misc.mapi (fun _ i -> Predicate.Proj (i, var)) pats), [])
    | _ -> ([], [])
		
let rec frame_to_subs frame i = 
	match frame with
		| Farrow (Some p, f1, f2, _) -> (C.expand bindpat [(p, Predicate.Var (get_ho_param i))] []
			) @ (frame_to_subs f2 (i+1))
		| _ -> []

(** n_supplied > 0, ho_n = 0, curr_n = 0 *)
let rec get_partial_app_subs fr n_supplied ho_n curr_n = 
	match fr with
	| Farrow (Some l, f1, f2, _) -> 
		(Pattern.bind_pexpr l (Predicate.Var (get_ho_param curr_n)))
			@ (get_partial_app_subs f2 n_supplied ho_n (curr_n+1))
	| Farrow (None, f1, f2, _) -> 
		if (curr_n = 0) then (* from n_supplied *) 
			((get_ho_param n_supplied, Predicate.Var (get_ho_param curr_n)))
				:: (get_partial_app_subs f2 n_supplied (n_supplied+1) (curr_n+1))
		else (* from curr_n *) 
			(get_ho_param ho_n, Predicate.Var (get_ho_param curr_n))
				:: (get_partial_app_subs f2 n_supplied (ho_n+1) (curr_n+1))
	| _ -> []	