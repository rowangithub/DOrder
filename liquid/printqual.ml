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

open Outcometree
open Format
open Location

let qualifier_names quals = List.map (fun (p, _, _) -> Path.name p) quals

let rec qualify_tree_of_type_scheme otyp fr =
  let qualify = qualify_tree_of_type_scheme in
  match (otyp, fr) with
      (Otyp_arrow(l, t1, t2), Frame.Farrow(_, f1, f2, _)) ->
        Otyp_arrow(l, qualify t1 f1, qualify t2 f2)
    | (Otyp_constr(id, tyl, None), Frame.Fconstr(_, fl, _, (_, Frame.Qconst quals),_)) ->
        let qualifier_num = [string_of_int (List.length quals)] in
        let qualifier_desc = if !Clflags.brief_quals then qualifier_num else
                              qualifier_names quals in
        Otyp_constr(id, List.map2 qualify tyl fl, Some qualifier_desc)
    | (Otyp_constr (id, tyl, _), Frame.Frecord (_, _, (_, Frame.Qconst quals))) ->
        Otyp_constr (id, tyl, Some (qualifier_names quals))
    | (Otyp_var _, Frame.Fvar _) ->
        otyp
    | (Otyp_tuple (ts, _), Frame.Ftuple (fs, (_, Frame.Qconst quals))) ->
        Otyp_tuple(List.map2 qualify ts fs, Some (qualifier_names quals))
    | (_, Frame.Funknown) ->
        otyp
    | _ -> assert false
