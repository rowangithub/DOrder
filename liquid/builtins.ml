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

open Longident
open Typedtree
open Predicate
open Frame
open Asttypes
open Types

let rec mk_longid = function
  | [] -> assert false
  | [id] -> Lident id
  | id :: idrem -> Ldot (mk_longid idrem, id)

let qsize funm rel x y z = (Path.mk_ident ("SIZE_" ^ (pprint_rel rel)), y,
                       Atom(Var z, rel, FunApp(funm, [Var x])))

let qsize_arr = qsize "Array.length" 
let qsize_str = qsize "String.length"

let qdim rel dim x y z =
  let dimstr = string_of_int dim in
    (Path.mk_ident ("DIM" ^ dimstr ^ (pprint_rel rel)), y,
     Atom(Var z, rel, FunApp("Bigarray.Array2.dim" ^ dimstr, [Var x])))

let qint rel i y =
  (Path.mk_ident (Printf.sprintf "INT_%s%d" (pprint_rel rel) i), y, Atom(Var y, rel, PInt i))

let qrel rel x y =
    (Path.mk_ident (Printf.sprintf "_%s%s%s_" (Path.name x) (pprint_rel rel) (Path.name y)), 
     x,
     Atom(Var x, rel, Var y))

let mk_tyvar () = Frame.Fvar(Path.mk_ident "'a")

let mk_int qs = Fconstr(Predef.path_int, [], [], ([], Qconst qs), [])

let uFloat = Fconstr(Predef.path_float, [], [], ([], Qconst []), [])
let uChar = Fconstr(Predef.path_char, [], [], ([], Qconst []), [])

let mk_string qs = Fconstr(Predef.path_string, [], [], ([], Qconst qs), [])

let uString = mk_string []
let rString name v p = mk_string [(Path.mk_ident name, v, p)]

let mk_bool qs = Fconstr(Predef.path_bool, [], [], ([], Qconst qs), [])
let uBool = mk_bool []
let rBool name v p = mk_bool [(Path.mk_ident name, v, p)]

let mk_array f qs = Fconstr(Predef.path_array, [f], [Invariant], ([], Qconst qs), [])

let find_constructed_type id env =
  let path =
    try fst (Env.lookup_type (mk_longid id) env)
    with Not_found -> Printf.printf "Couldn't load %s!\n" (String.concat " " id); assert false
  in
  let decl = Env.find_type path env in (path, List.map translate_variance decl.type_variance)

let mk_named id fs qs env =
  let (path, varis) = find_constructed_type id env in Fconstr(path, fs, varis, ([], Qconst qs), [])

let mk_ref f env = Frecord (fst (find_constructed_type ["ref"; "Pervasives"] env), [(f, "contents", Mutable)], ([], Qconst []))

let mk_bigarray_kind a b qs env = mk_named ["kind"; "Bigarray"] [a; b] qs env

let mk_bigarray_layout a qs env = mk_named ["layout"; "Bigarray"] [a] qs env

let mk_bigarray_type a b c qs env = mk_named ["t"; "Array2"; "Bigarray"] [a; b; c] qs env

let mk_unit () = Fconstr(Predef.path_unit, [], [], ([], Qconst []), [])
let uUnit = mk_unit ()

let uInt = mk_int []
let rInt name v p = mk_int [(Path.mk_ident name, v, p)]
let rArray b name v p = mk_array b [(Path.mk_ident name, v, p)]

let mk_list f qs = Fconstr(Predef.path_list, [f], [Frame.Covariant], ([], Qconst qs), [])
let uList f = mk_list f [] 
let rList name v p f = mk_list f [(Path.mk_ident name, v, p)]

let char = ref 0

let reset_idents () =
  char := Char.code 'a' - 1

let fresh_ident () =
  incr char; String.make 1 (Char.chr (!char))

let def f =
  let (x, y) = (Path.mk_ident (fresh_ident ()), Path.mk_ident (fresh_ident ())) in
  let (f, fy) = f x in
  let xid = match x with
  | Path.Pident id -> id
  | _ -> assert false
  in Farrow (Some (Tpat_var xid), f, fy y, [])

let defun f =
  reset_idents (); def f

let (==>) x y = (x, y)

let (===>) x y = x ==> fun _ -> def y

let forall f = f (Frame.Fvar(Path.mk_ident "'a"))

let op_frame path qname op =
  (path, defun (fun x -> uInt ===>
                fun y -> uInt ==>
                fun z -> rInt qname z (Var z ==. Binop (Var x, op, Var y))))


let tag_function = "__tag"

let tag x = FunApp(tag_function, [x])

let or_frame () =
   defun (fun x -> uBool ===>
          fun y -> uBool ==>
          fun z -> rBool "||" z
            (((tag (Var z) ==. PInt 1) &&. ((tag (Var x) ==. PInt 1) ||. (tag (Var y) ==. PInt 1))) ||.
             ((tag (Var z) ==. PInt 0) &&. (tag (Var x) ==. PInt 0) &&. (tag (Var y) ==. PInt 0))))

let and_frame () =
   defun (fun x -> uBool ===>
          fun y -> uBool ==>
          fun z -> rBool "&&" z (tag (Var z) <=>. ((tag (Var x) ==. PInt 1) &&. (tag (Var y) ==.  PInt 1))))

let qbool_rel qname rel (x, y, z) = rBool qname z (tag (Var z) <=>. Atom (Var x, rel, Var y))

let poly_rel_frame path qname rel =
  (path,
   defun (forall (fun a -> fun x -> a ===> fun y -> a ==> fun z -> qbool_rel qname rel (x, y, z))))

let _frames = lazy([
  op_frame ["+"; "Pervasives"] "+" Plus;
  op_frame ["-"; "Pervasives"] "-" Minus;
  op_frame ["/"; "Pervasives"] "/" Div;
  op_frame ["*"; "Pervasives"] "*" Times;

	(["~-"; "Pervasives"],
   defun (fun x -> uInt ==> fun y -> rInt "~-" y ((Var y) ==. (PInt 0 -- (Var x)))));
	
  (["lsr"; "Pervasives"],
   defun (fun x -> uInt ===> fun y -> uInt ==> fun z -> rInt "lsr" z (PInt 0 <=. Var z)));

  (["land"; "Pervasives"],
   defun (fun x -> uInt ===>
          fun y -> uInt ==>
          fun z ->
            rInt "land" z
              (((Var x >=. PInt 0) &&. (Var y >=. PInt 0))
                 =>. big_and [PInt 0 <=. Var z; Var z <=. Var x; Var z <=. Var y;])));

  (["mod"; "Pervasives"],
   defun (fun x -> uInt ===>
          fun y -> uInt ==>
          fun z -> (rInt "mod" z
            (
            (Var z ==. (Binop (Var x, Mod, Var y)))))));														

  (["/"; "Pervasives"],
   defun (fun x -> uInt ===>
          fun y -> uInt ==>
          fun z ->
            rInt "/" z
              ((Var z ==. Var x /- Var y) &&. (Var z ==. (Var x /- Var y) +- PInt 0))));

  (["&&"; "Pervasives"], and_frame ());

  (["||"; "Pervasives"], or_frame ());

  (["or"; "Pervasives"], or_frame ());

  (["not"; "Pervasives"],
   defun (fun x -> uBool ==> fun y -> rBool "NOT" y (tag (Var y) <=>. (tag (Var x) ==. PInt 0))));

  (["ignore"; "Pervasives"], defun (forall (fun a -> fun x -> a ==> fun y -> uUnit)));

  (["succ"; "Pervasives"], defun (fun x -> uInt ==> fun y -> rInt "succ" y ((Var y) ==. ((Var x) +- PInt 1))));

  (["pred"; "Pervasives"], defun (fun x -> uInt ==> fun y -> rInt "pred" y ((Var y) ==. ((Var x) -- PInt 1))));

	(["@"; "Pervasives"], defun (forall (fun a -> fun x -> uList a ===> fun y -> uList a ==> fun z -> 
		rList "@" z (FunApp ("List.length", [Var z]) ==. FunApp ("List.length", [Var x]) +- FunApp ("List.length", [Var y])) a
		)));

  poly_rel_frame ["="; "Pervasives"] "=" Eq;
  poly_rel_frame ["=="; "Pervasives"] "==" Eq;
  poly_rel_frame ["!="; "Pervasives"] "!=" Ne;
  poly_rel_frame ["<>"; "Pervasives"] "<>" Ne;
  poly_rel_frame ["<"; "Pervasives"] "<" Lt;
  poly_rel_frame [">"; "Pervasives"] ">" Gt;
  poly_rel_frame [">="; "Pervasives"] ">=" Ge;
  poly_rel_frame ["<="; "Pervasives"] "<=" Le;

  (["length"; "Array"],
   defun (forall (fun a ->
          fun x -> mk_array a [] ==>
          fun y -> mk_int [qsize_arr Eq x y y; qint Ge 0 y])));

  (["set"; "Array"],
   defun (forall (fun a ->
          fun x -> mk_array a [] ===>
          fun y -> mk_int [qsize_arr Lt x y y; qint Ge 0 y] ===>
          fun z -> a ==>
          fun _ -> 
						if (!Clflags.no_effect) then uUnit else
						Frame.push_effect_fst ([(x, mk_array a [
							let test_var = Path.mk_ident "AA" in
							(Path.mk_ident "Effect",test_var,
							Predicate.big_and [
								Predicate.implies (
									Predicate.Atom (Var (Frame.get_ho_param 0),Predicate.Eq, Var y), 
									Predicate.Atom (FunApp("Ret2", [Var test_var; Var (Frame.get_ho_param 0)]), Predicate.Eq, Var z));
								Predicate.implies (
									Predicate.Atom (Var (Frame.get_ho_param 0),Predicate.Ne, Var y),
									Predicate.Atom (FunApp("Ret2", [Var test_var; Var (Frame.get_ho_param 0)]), 
										Predicate.Eq, FunApp("Ret2", [Var x; Var (Frame.get_ho_param 0)])));
								Predicate.Atom (FunApp("Array.length", [Var test_var]), Predicate.Eq, FunApp("Array.length", [Var x]))	
							])])]) uUnit)));

  (["get"; "Array"],
   defun (forall (fun a ->
          fun x -> mk_array a [] ===>
          fun y -> mk_int [qsize_arr Lt x y y; qint Ge 0 y] ==>
          fun _ -> a)));

  (["make"; "Array"],
   defun (forall (fun a ->
          fun x -> rInt "NonNegSize" x (PInt 0 <=. Var x) ===>
          fun y -> a ==>
          fun z -> mk_array a [qsize_arr Eq z z x])));

  (["init"; "Array"],
   defun (forall (fun a ->
          fun x -> rInt "NonNegSize" x (PInt 0 <=. Var x) ===>
          fun i -> (defun (fun y -> rInt "Bounded" y ((PInt 0 <=. Var y) &&. (Var y <. Var x)) ==> fun _ -> a)) ==>
          fun z -> mk_array a [qsize_arr Eq z z x])));

  (["copy"; "Array"],
   defun (forall (fun a ->
          fun arr -> mk_array a [] ==>
          fun c -> rArray a "SameSize" c (FunApp("Array.length", [Var c]) ==. FunApp("Array.length", [Var arr])))));

  (["make"; "String"],
   defun (forall (fun a ->
          fun x -> rInt "NonNegSize" x (PInt 0 <=. Var x) ===>
          fun c -> uChar ==>
          fun s -> mk_string [qsize_str Eq s s x])));

  (["length"; "String"],
   defun (fun x -> mk_string [] ==>
          fun y -> mk_int [qsize_str Eq x y y; qint Ge 0 y]));

  (["get"; "String"],
   defun (forall (fun a ->
          fun x -> uString ===>
          fun y -> mk_int [qsize_str Lt x y y; qint Ge 0 y] ==>
          fun z -> uChar)));

  (["int"; "Random"], defun (fun x -> rInt "PosMax" x (PInt 0 <. Var x) ==>
                             fun y -> rInt "RandBounds" y ((PInt 0 <=. Var y) &&. (Var y <. Var x))));
														
	(["bool"; "Random"], defun (fun x -> uUnit ==>
                             fun y -> rBool "RandBool" y True));													
])

let bigarray_dim_frame dim env =
  (["dim" ^ string_of_int dim; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun r -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ==>
          fun s -> mk_int [qdim Eq dim r s s; qint Gt 0 s])))

let _lib_frames env = [
  (["create"; "Array2"; "Bigarray"],
   defun (forall (fun a -> forall (fun b -> forall (fun c ->
          fun k -> mk_bigarray_kind a b [] env ===>
          fun l -> mk_bigarray_layout c [] env ===>
          fun dim1 -> mk_int [qint Gt 0 dim1] ===>
          fun dim2 -> mk_int [qint Gt 0 dim2] ==>
          fun z -> mk_bigarray_type a b c [qdim Eq 1 z z dim1; qdim Eq 2 z z dim2] env)))));

  (["get"; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun r -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ===>
          fun i -> mk_int [qint Ge 0 i; qdim Lt 1 r i i] ===>
          fun j -> mk_int [qint Ge 0 j; qdim Lt 2 r j j] ==>
          fun _ -> a)));

  (["set"; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun u -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ===>
          fun i -> mk_int [qint Ge 0 i; qdim Lt 1 u i i] ===>
          fun j -> mk_int [qint Ge 0 j; qdim Lt 2 u j j] ===>
          fun v -> a ==>
          fun _ -> uUnit)));

  bigarray_dim_frame 1 env;
  bigarray_dim_frame 2 env;
]

let _type_path_constrs env = [
  ("ref", find_constructed_type ["ref"; "Pervasives"] env);
  ("array2", find_constructed_type ["t"; "Array2"; "Bigarray"] env);
]

let _type_paths = ref None

let ext_find_type_path t =
  let (_, (path, _)) = (List.find (fun (a, _) -> (a = t))
                          (match !_type_paths with None -> assert false
                             | Some b -> b))
  in path

let find_path id env = fst (Env.lookup_value (mk_longid id) env)

let frames env =
  let _ = _type_paths := Some (_type_path_constrs env) in
  let resolve_names x = List.map (fun (id, fr) -> (find_path id env, fr)) x in
  List.append (resolve_names (Lazy.force _frames)) (resolve_names (_lib_frames env))

let equality_qualifier exp =
  let x = Path.mk_ident "V" in
    let pred = Var x ==. exp in
    Predicate.pprint Format.str_formatter pred;
    let expstr = Format.flush_str_formatter () in (Path.mk_ident expstr, x, pred)

let equality_refinement exp = ([], Qconst [equality_qualifier exp])

let tag_refinement t =
  let x = Path.mk_ident "V" in
    let pred = tag (Var x) ==. PInt t in
    Predicate.pprint Format.str_formatter pred;
    let expstr = Format.flush_str_formatter () in
      ([], Qconst [(Path.mk_ident expstr, x, pred)])

let size_lit_refinement i =
  let x = Path.mk_ident "x" in
    ([], Qconst [(Path.mk_ident "<size_lit_eq>",
                  x,
                  FunApp("Array.length", [Var x]) ==. PInt i)])
									
									
let size_lit_sorted_refinement i = 									
	let x = Path.mk_ident "x" in
    ([], Qconst [(Path.mk_ident "<size_lit_eq>",
		  x,
		  And ((FunApp("Array.length", [Var x]) ==. PInt i), 
			(implies ((PInt 0 <=. Var (Frame.get_ho_param 0)) &&. 
				(Var (Frame.get_ho_param 0) <. (PInt i -- PInt 1)), 
						FunApp ("Ret2", [Var x; Var (Frame.get_ho_param 0)]) <=. 
									FunApp ("Ret2", [Var x; PInt 1 +- Var (Frame.get_ho_param 0)]))))
			)])
									
let field_eq_qualifier name pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<field_eq>", x, Field (name, Var x) ==. pexp)

let proj_eq_qualifier n pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<tuple_nth_eq>", x, Proj (n, Var x) ==. pexp)
