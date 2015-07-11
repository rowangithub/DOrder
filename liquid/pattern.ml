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

open Typedtree

module P = Predicate
module C = Common

let is_deep = function
  | Tpat_any
  | Tpat_var _ -> false
  | _ -> true

let pattern_descs = List.map (fun p -> p.pat_desc)

let _bind_vars = function
  | (Tpat_any, _) -> ([], [])
  | (Tpat_var x, Tpat_var y) -> ([], [(x, y)])
  | (Tpat_tuple p1s, Tpat_tuple p2s)
  | (Tpat_construct (_, p1s), Tpat_construct (_, p2s)) ->
      (List.combine (pattern_descs p1s) (pattern_descs p2s), [])
  | _ -> assert false

let bind_vars p1 p2 = C.expand _bind_vars [(p1, p2)] []

let rec fold f b p = match p with
  | Tpat_any
  | Tpat_var _ -> f b p
  | Tpat_tuple pats ->
      let b = List.fold_left (fold f) b (pattern_descs pats) in f b p
  | _ -> assert false

let null_binding_fold b = function
  | Tpat_var x -> (Path.Pident x, P.Var (Path.mk_ident "z")) :: b
  | _ -> b

let null_binding b pat =
  fold null_binding_fold b pat

let bind_pexpr pat pexp =
  let rec bind_rec subs (pat, pexp) =
    match pat with
    | Tpat_any -> subs
    | Tpat_var x -> (Path.Pident x, pexp) :: subs
    | Tpat_tuple pats ->
      let pexps = Misc.mapi (fun pat i -> (pat.pat_desc, P.Proj(i, pexp))) pats in
        List.fold_left bind_rec subs pexps
    | _ -> null_binding_fold subs pat
  in bind_rec [] (pat, pexp)

let desugar_bind pat pexp =
  P.big_and (List.map (fun (x, exp) -> P.equals(P.Var x, exp)) (bind_pexpr pat pexp))

let rec same p1 p2 =
  match (p1, p2) with
  | (Tpat_var x, Tpat_var y) when x = y -> true
  | (Tpat_any, Tpat_any) -> true
  | (Tpat_tuple pats1, Tpat_tuple pats2) ->
      List.for_all2 same (pattern_descs pats1) (pattern_descs pats2)
  | _ -> false
