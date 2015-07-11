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

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error ->
      fprintf ppf "Preprocessor error"
  | Env.Error err ->
      Env.report_error ppf err
  | Ctype.Tags(l, l') -> fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, err) ->
      Location.print ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, err) ->
      Location.print ppf loc; Typeclass.report_error ppf err
  | Includemod.Error err ->
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print ppf loc; Typemod.report_error ppf err
  | Qualifymod.Error(loc, err) ->
      Location.print ppf loc; Qualifymod.report_error ppf err
  | Qualifymod.Errors(el) ->
      Qualifymod.report_errors ppf el
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn

