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

open Config
open Format
open Parsetree
open Asttypes

module P = Predicate
module C = Common

module QS = Set.Make(struct
                       type t = string * string * P.t
                       let compare = compare
                     end)

let patf = ref ""
            
let expand_quals env qstrs prgids =
  let expand_squal (name, pat) =
    Qualdecl.transl_pattern_valu env prgids name pat
  in
  C.flap (expand_squal) qstrs 

let dump_qset ppf qs =
  QS.iter (fun (nm, v, q) -> fprintf ppf "@[squalif@ %s(%s)@ :@ %a@.@]" nm v P.pprint q) qs

let dump_default_qualifiers source qname =
  let qf = formatter_of_out_channel (open_out qname) in
  let _ = pp_set_margin qf 1230912 in
  let _ = C.verbose_level := C.ol_dquals in
  let (str, env, fenv) = source in
  let prgids = Qualgen.visit_sstr str in
  let dqstrs = Pparse.file std_formatter !patf Parse.qualifier_patterns ast_impl_magic_number in
  let dqstrs = expand_quals env dqstrs prgids in
  let qs = List.fold_left (fun qs q -> QS.add q qs) QS.empty dqstrs in
    dump_qset qf qs; pp_print_flush qf ()
