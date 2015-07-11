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

open Types
open Builtins
open Frame

let same_shape = same_shape false

let measures = ref (Hashtbl.create 0)

let udt_table = ref (Hashtbl.create 0)

let qframe = Frame.Fvar (Path.mk_ident "quantifier")

(** Check if an old array is bounded to an array inscope *)	
let well_bounded_oldarray env v = 
	if (Common.str_contains (Path.name v) (Frame.old_array_flag)) then
		let name = String.sub (Path.unique_name v) (String.length (Frame.old_array_flag)) 
				(String.length (Path.unique_name v) - String.length (Frame.old_array_flag)) in
		List.exists (fun key -> String.compare name (Path.unique_name key) = 0) (Lightenv.domain env)
	else false

let find_or_fail var env = 
	try Lightenv.find var env with Not_found -> 
		(Format.fprintf Format.std_formatter "%s is unfound@." (Path.unique_name var);
		(*if (well_bounded_oldarray env var) then uInt
		else*) assert false)

let constr_app_shape paths out_shape in_shapes = 
  let f i o = 
    match o with
      Fconstr(a, _, _, _, _) ->
        Path.same i a 
      | _ -> false
  in if (List.length paths = List.length in_shapes) && 
        (List.for_all2 f paths in_shapes) 
          then out_shape else Funknown

(* ext_find_type_path isn't initialized until sometime late in ocaml startup, so
   we'll suspend this and force it when we know it's safe *)
let fun_app_shapes = lazy(
  let array2_path = Builtins.ext_find_type_path "array2" in
    [("Array.length", constr_app_shape [Predef.path_array] uInt);
		 ("List.length", constr_app_shape [Predef.path_list] uInt);
     ("Bigarray.Array2.dim1", constr_app_shape [array2_path] uInt);
     ("Bigarray.Array2.dim2", constr_app_shape [array2_path] uInt);
     (Builtins.tag_function, (function [Fconstr _] -> uInt | _ -> Funknown))]@
		(Hashtbl.fold (fun p ms res -> 
			res @ (List.map (fun (m, _) -> (Path.name m, constr_app_shape [p] uInt)) ms)	
		) (!measures) [])
)

let pred_is_well_typed env p =
	let rec get_expr_shape env p = match p with
  	| Predicate.PInt _ -> uInt
  	| Predicate.Var x -> find_or_fail x env
  	| Predicate.FunApp (s, p') -> 
			if (Common.str_contains s "Ret" || Common.str_contains s "Arg" ) then uInt
			else (List.assoc s (Lazy.force fun_app_shapes)) (List.map (get_expr_shape env) p')
  	| Predicate.Binop (p1, op, p2) ->
      	let p1_shp = get_expr_shape env p1 in
      	let p1_int = same_shape p1_shp uInt in
      	let p2_shp = get_expr_shape env p2 in
      	let p2_int = same_shape p2_shp uInt in
      	if p1_int && p2_int then uInt else Funknown
  	| Predicate.Field (name, r) ->
      	begin match get_expr_shape env r with
        	| Frecord (_, fs, _) ->
            	let is_referenced_field (_, name2, _) = String.compare name name2 = 0 in
              if List.exists is_referenced_field fs then
                (match (List.find is_referenced_field fs) with (f, _, _) -> f)
              else Funknown
        	| f -> Funknown
      	end
  	| Predicate.Proj (n, t) ->
      	begin match get_expr_shape env t with
        	| Ftuple (fs, _) -> (try List.nth fs n with Failure _ -> Funknown)
        	| _ -> Funknown
      	end
  and pred_shape_is_bool env p = match p with
	  | Predicate.True -> true
	  | Predicate.Not p -> pred_shape_is_bool env p 
	  | Predicate.Or (p1, p2)  
	  | Predicate.And (p1, p2) -> (pred_shape_is_bool env p1) && (pred_shape_is_bool env p2)
	  | Predicate.Atom (p1, rel, p2) -> 
		  let p1_shp = 
				if (List.exists (fun heop -> 
					List.exists (fun var -> Path.same heop var) (Predicate.exp_vars p1)
					) (Frame.get_all_ho_params () @ Frame.get_all_ex_params ())) then uInt 
				else get_expr_shape env p1 in
		  let p2_shp =  
				if (List.exists (fun heop -> 
					List.exists (fun var -> Path.same heop var) (Predicate.exp_vars p2)
					) (Frame.get_all_ho_params () @ Frame.get_all_ex_params ())) then uInt
				else get_expr_shape env p2 in
	    begin match rel with
	    | Predicate.Ne
	    | Predicate.Eq ->
	     ((same_shape p1_shp p2_shp) && not(same_shape p1_shp Funknown))
	     || ((same_shape p1_shp uBool) && (same_shape p2_shp uInt))
	     || ((same_shape p1_shp uInt) && (same_shape p2_shp uBool))
			 || ((same_shape p1_shp qframe) || (same_shape p2_shp qframe))
	    | Predicate.Gt
	    | Predicate.Ge
	    | Predicate.Lt
	    | Predicate.Le ->
			let r = ((same_shape p1_shp p2_shp) && (same_shape p1_shp uInt || 
	                                   (function Fvar _ -> true | _ -> false) p1_shp ||
	                                   same_shape p1_shp uFloat)) ||
			(same_shape p1_shp qframe) || (same_shape p2_shp qframe) in r
	    end
  	| Predicate.Iff (px, q) -> same_shape (get_expr_shape env px) uInt && pred_shape_is_bool env q
		(* check 1) u, v are in-scop 2) u, v are of type t 3) f is a link field of type t *)
		| Predicate.Link (d, cons, _, u, v)	->
			let d_shp = get_expr_shape env d in
			(match d_shp with 
				| Frame.Fconstr (d_type, ds,_,_,_) -> 
					if (Hashtbl.mem (!measures) d_type) then (
						let declaration = Hashtbl.find (!udt_table) d_type in
						let cons = String.capitalize cons in 
						match declaration.type_kind with 
							| Type_variant decs -> (List.mem_assoc cons decs)
							| kind -> assert false
					)
					else if (Path.same d_type Predef.path_list) then (
						(* Our type system supports a list of containers *)
						(*String.compare cons "cons" = 0*)
						if (ds = []) then String.compare cons "cons" = 0
						else
							let d = List.hd ds in
							match d with
								| Frame.Fconstr (d_type, _,_,_,_) when (Hashtbl.mem (!measures) d_type) -> 
									(let declaration = Hashtbl.find (!udt_table) d_type in
									let cons = String.capitalize cons in 
									match declaration.type_kind with 
										| Type_variant decs -> (List.mem_assoc cons decs)
										| kind -> assert false )
								| _ -> String.compare cons "cons" = 0
					)
					else false
					(*Hashtbl.mem (!measures) d_type || Path.same d_type Predef.path_list*)
				| _ -> false)
		| Predicate.Reach (d, u) ->
			let d_shp = get_expr_shape env d in
			(*let u_shp = get_expr_shape env u in*)
			(match d_shp with 
				| Frame.Fconstr (d_type, _,_,_,_) -> 
					Hashtbl.mem (!measures) d_type || Path.same d_type Predef.path_list
					(*if (Hashtbl.mem (!measures) d_type || Path.same d_type Predef.path_list) then
						match u_shp with Funknown -> true | _ -> true
					else false*)
				| _ -> false)
		| Predicate.Forall (ps, p) -> 
			let env = Lightenv.addn (List.map (fun p -> 
				(p, qframe)) ps) env in
			pred_shape_is_bool env p 
		| Predicate.Bool b -> 	
			let b_shp = get_expr_shape env b in
			match b_shp with
				| Frame.Fconstr (b_type, _,_,_,_) when Path.same b_type Predef.path_bool -> true
				| _ -> false in 
	pred_shape_is_bool env p

let refinement_well_formed env solution r qual_expr =
  let pred = refinement_predicate (Hashtbl.create 0) solution qual_expr r in
  let var_bound v = 
		(Lightenv.mem v env || List.exists (fun hop -> Path.same v hop) 
		(Frame.get_all_ho_params ()) || List.exists (fun ep -> Path.same v ep)
		(Frame.get_all_ex_params ()) || well_bounded_oldarray env v) in
  let well_scoped = List.for_all var_bound (Predicate.vars pred) in
	let well_typed = if (well_scoped) then 
		let r = pred_is_well_typed env pred in r
		else false in
  let result = well_scoped && well_typed in result
