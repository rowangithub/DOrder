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
open Typedtree

type t = Path.t * Path.t * Predicate.t

let compare = compare

let pprint ppf (_, _, pred) = Predicate.pprint ppf pred

let apply x (_, y, p) = Predicate.subst x y p

exception Refinement_not_closed

(* The user specifies qualifiers as open predicates - i.e., the variables named
   in the qualifier may not yet be in scope at the time of definition.  But
   we want qualifiers that refer to OCaml paths, which are unique, not
   variable names, which can appear multiple times.  Using variable names
   instead of unique paths would cause trouble with the following expression:

   let a = 1 in
   let x = a + 1 in   (* x has type {v : int | v > a} *)
   let a = 3 in ...   (* x's type is now incorrect *)

   This function replaces all instances of named variables in a qualifier with
   the unique paths of the same name in the given environment.  It raises
   Refinement_not_closed if a variable in the qualifier is not found in the
   environment. *)
let instantiate varmap (path, valu, pred) =
	(* Don't instantiate the quantifiers *)
	let quantifiers = Predicate.quantifiers pred in
	let quantifiers = Common.remove_customized_duplicates (
		fun q1 q2 -> String.compare (Path.name q1) (Path.name q2) = 0) quantifiers in
		
	(*let _ = Format.fprintf Format.std_formatter "pred = %a@." Predicate.pprint pred in
	let _ = List.iter (fun (str, path) -> 
		Format.fprintf Format.std_formatter "str = %s and path = %s@." 
		str (Path.unique_name path)
		) varmap in*)	
		
	let quantifiers = (List.map (fun quantifier -> 
		let name = Path.ident_name_crash quantifier in
		(*let _ = Format.fprintf Format.std_formatter "name = %s@." name in*)
		let _ = assert (not (List.mem_assoc name varmap)) in
		name, quantifier
		) quantifiers) in
	let varmap = quantifiers @ varmap (*@ [("cons", Path.Pident (Ident.create_persistent "cons"))]*) 
	in
	(* instantiate the quantifiers (with its types) properly *)
	let pred = Predicate.map_quantifiers (fun path ->
		try List.assoc (Path.ident_name_crash path) (quantifiers) 
		with _ -> path
		) pred in	
  (* Don't instantiate the bound variable *)
  let varmap = (Path.ident_name_crash valu, valu) :: varmap in
  try Some (path, valu, Predicate.instantiate_named_vars varmap pred)
  with Not_found -> None
			
(* Judge whether two qualifeirs are equivalent *)		
let equals q1 q2 = 
	match (q1, q2) with
		| (name1, valu1, p1), (name2, valu2, p2) ->
			let name1 = Path.name name1 in
			let name2 = Path.name name2 in
			let pre = Str.regexp_string "Pre" in
			let post = Str.regexp_string "Post" in
			let process = 
				if not !(Clflags.reachability) then false else (*Fix me!!*)
				((try ignore (Str.search_backward pre name1 (String.length name1 - 1)); true 
						with Not_found -> false) &&
				(try ignore (Str.search_backward post name2 (String.length name2 - 1)); true 
						with Not_found -> false) ) ||
				((try ignore (Str.search_backward pre name2 (String.length name2 - 1)); true 
						with Not_found -> false) &&
				(try ignore (Str.search_backward post name1 (String.length name1 - 1)); true 
						with Not_found -> false) ) in
			if (process) then false
			else if (Path.same valu1 valu2) then p1 = p2
			else (Predicate.subst (Predicate.Var valu2) valu1 p1) = p2