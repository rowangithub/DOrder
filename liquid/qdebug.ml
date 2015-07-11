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

open Qualifymod
open Btype
open Typedtree
open Parsetree
open Asttypes
open Types
open Format
open Common

let print_id id =
  String.concat "." (Longident.flatten id)

let rec pprint_expression ppf exp =
  let pprint_binding ppf (lid, exp) =
    fprintf ppf "@[%s =@;<1 2>%a@]" (print_id lid) pprint_expression exp
  in
  let rec pprint_exp ppf e =
    match e.pexp_desc with
      | Pexp_constraint (_, _, _) ->
          fprintf ppf ": ty"
      | Pexp_constant (Const_int n) ->
          fprintf ppf "%d" n
      | Pexp_constant (Const_float f) ->
          fprintf ppf "%s" f
      | Pexp_constant (Const_char c) ->
          fprintf ppf "%c" c
      | Pexp_constant (Const_string s) ->
          fprintf ppf "%s" s
      | Pexp_ident id ->
          fprintf ppf "%s" (print_id id)
      | Pexp_construct (tag, eopt, _) ->
          let tagstr = print_id tag in
            begin match eopt with
              | None -> fprintf ppf "%s" tagstr
              | Some e -> fprintf ppf "%s@ %a" tagstr pprint_expression e
            end
      | Pexp_ifthenelse (e1, e2, Some e3) ->
          fprintf ppf "@[if@ %a@ then@;<1 4>%a@;<1 0>@[else@;<1 4>%a@]@]"
            pprint_expression e1 pprint_expression e2 pprint_expression e3
      | Pexp_function (_, _, [(pat, e)]) ->
          fprintf ppf "@[fun %a ->@;<1 2>%a@]" pprint_pattern pat pprint_expression e
      | Pexp_apply (e1, exps) ->
          let pprint_arg ppf (_, e) =
            fprintf ppf "(%a)" pprint_expression e
          in fprintf ppf "@[(%a@;<1 2>%a)@]" pprint_expression e1 (pprint_list "" pprint_arg) exps
      | Pexp_let (recf, binds, e2) ->
          fprintf ppf "@[let%a@ %a@;<1 0>in@;<1 2>%a@]"
               pprint_rec recf pprint_binds binds pprint_expression e2
      | Pexp_array es ->
          fprintf ppf "@[[|%a|]@]" (pprint_list ";" pprint_expression) es
      | Pexp_sequence(e1, e2) ->
          fprintf ppf "@[%a@]" (pprint_list ";" pprint_expression) [e1; e2]
      | Pexp_tuple(es) ->
          fprintf ppf "@[(%a)@]" (pprint_list "," pprint_expression) es
      | Pexp_record (bindings, None) ->
          fprintf ppf "@[{%a}@]" (pprint_list ";" pprint_binding) bindings
      | Pexp_field (e, lid) ->
          fprintf ppf "@[%a.%s@]" pprint_expression e (print_id lid)
      | Pexp_assertfalse ->
          fprintf ppf "assert@ false"
      | Pexp_assert e ->
          fprintf ppf "assert@ %a" pprint_expression e
      | Pexp_match (e, pel) ->
          fprintf ppf "match@ %a@ with@;<1 2>%a" pprint_expression e pprint_cases pel  
      | Pexp_try _ -> fprintf ppf "Pexp_try"
      | Pexp_variant _ -> fprintf ppf "Pexp_variant"
      | Pexp_setfield _ -> fprintf ppf "Pexp_setfield"
      | Pexp_when _ -> fprintf ppf "Pexp_when"
      | Pexp_send _ -> fprintf ppf "Pexp_send"
      | Pexp_lazy _ -> fprintf ppf "Pexp_lazy"
      | Pexp_poly _ -> fprintf ppf "Pexp_poly"
      | Pexp_object _ -> fprintf ppf "Pexp_object"                   
      | Pexp_while _ -> fprintf ppf "Pexp_while"
      | Pexp_for _ -> fprintf ppf "Pexp_for"
      | Pexp_new _ -> fprintf ppf "Pexp_new"
      | Pexp_setinstvar _ -> fprintf ppf "Pexp_setinstvar"
      | Pexp_letmodule _ -> fprintf ppf "Pexp_letmodule"
      | Pexp_record _ -> fprintf ppf "Pexp_record Some"
      | Pexp_function _ -> fprintf ppf "Pexp_function multi-arg"
      | Pexp_ifthenelse _ -> fprintf ppf "Pexp_ifthenelse no else"
      | _ -> assert false
  in fprintf ppf "@[%a@]" pprint_exp exp
and pprint_rec ppf = function
  | Recursive -> fprintf ppf "@ rec"
  | Nonrecursive -> fprintf ppf ""
  | _ -> assert false
and pprint_pat_list ppf ls =
  match ls with
    t::[] -> fprintf ppf "%a" pprint_pattern t
  | t::rem -> fprintf ppf "%a,@ %a" pprint_pattern t pprint_pat_list rem
  | [] -> fprintf ppf ""
and pprint_pattern ppf pat =
  match pat.ppat_desc with
    Ppat_any -> fprintf ppf "_"
  | Ppat_var x -> fprintf ppf "%s" x
  | Ppat_tuple ts -> fprintf ppf "(%a)" pprint_pat_list ts
  | Ppat_constraint (p, _) -> pprint_pattern ppf p
  | Ppat_constant (Const_int n) -> fprintf ppf "%d" n
    (* Pat knows more about how this generalizes *)
  | Ppat_construct (id, e, _) ->
      let id = String.concat "" (Longident.flatten id) in
      begin
      match id with
      | "::" -> 
          begin
          match e with 
          | Some ({ppat_desc = Ppat_tuple el}) ->
              let e1 = List.hd el in
              let e2 = List.nth el 1 in 
              fprintf ppf "%a::%a" pprint_pattern e1 pprint_pattern e2
          | _ -> assert false          
          end
      | "[]" ->
          fprintf ppf "[]"
      | cons ->
          begin
          match e with
          | Some ({ppat_desc = Ppat_tuple el}) ->
              fprintf ppf "(%a)" pprint_pat_list el
          | Some ({ppat_desc = Ppat_var v}) ->
              fprintf ppf "%s" v
          | None ->
              fprintf ppf ""
          | _ ->
              assert false
          end
      end
  | _ -> assert false
and pprint_and ppf = function
  | [] -> fprintf ppf "@ "
  |  _ -> fprintf ppf "@ and@ " 
and pprint_binds ppf = function
  | (pat, e)::rem ->
    fprintf ppf "@[%a@ =@;<1 2>%a%a%a@]" pprint_pattern pat pprint_expression e pprint_and rem pprint_binds rem
  | [] -> fprintf ppf ""
and pprint_cases ppf = function
  | (pat, e)::rem ->
    fprintf ppf "@[| %a@ ->@;<1 2>%a@\n%a@]" pprint_pattern pat pprint_expression e pprint_cases rem
  | [] -> fprintf ppf ""

let rec pprint_structure ppf str = 
  match str.pstr_desc with
  | Pstr_eval(exp) ->
      pprint_expression ppf exp
  | Pstr_value(recursive, pl) ->
      fprintf ppf "@[let%a@ %a@]" pprint_rec recursive pprint_binds pl
  | _ -> assert false
