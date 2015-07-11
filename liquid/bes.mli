(* $Id: promela.mli 2996 2012-03-14 09:58:12Z weissmam $

Copyright (c) 2011 - 2012 Technische Universitaet Muenchen
Copyright (c) 2011 Alexander Ostrovsky <ostrovsk@in.tum.de
Copyright (c) 2011 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Apple Inc. nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

(** Operations for minimization of Boolean expressions. *)

(** A boolean type with 3 values : true, false and don't care *)
type ext_bool = [ `True | `False | `Dontcare ]

(** A minterm with true, false and dont't care values. *)
type dnf_minterm = ext_bool list

(** A sum of minterms. *)
type dnf_expression = dnf_minterm list

(** The algorithm for the optimization. *)
type optimization_algorithm =
  | Qmc
  (** Only Quine-McCluskey. *)

  | Qmc_CyclicCore
  (** Quine-McCluskey, then cyclic core. *)

  | Qmc_SimpleHeuristic_SortOnce
  (** Quine-McCluskey, then cyclic core, then heuristic cover search.
    The impicants are sorted by the number of covered minterms once
    before the optimization. The length of the implications is used
    as the heuristic value.  *)

  | Qmc_SimpleHeuristic_SortEachTime
  (** Quine-McCluskey, then cyclic core, then heuristic cover search.
    The impicants are sorted by the number of covered minterms after
    each step. The length of the implications is used as the
    heuristic value. *)

  | Qmc_SimpleHeuristic_AdvancedHeuristic
  (** Quine-McCluskey, then cyclic core, then heuristic cover search.
    The impicants are sorted after each step.
    Advanced heuristic formula (see thesis) is used
    to calculate the heuristic value *)

  | Qmc_SimpleHeuristic_Best
  (** Quine-McCluskey, then cyclic core, then heuristic cover search.
    The impicants are sorted by after each step.
    Advanced heurisitc or the length of the
    implications is used as heuristic value, depending on the
    better results. *)

  | Qmc_PetricksMethod
  (** Quine-McCluskey and then Petrick's method. *)

  | In_Place_Heuristic
  (** EXPREREMNTAL, NOT TESTED: A heuristic optimization method that
    uses the Expand and Irrendundant (Here simple heuristic with
    advanced heuristic formaula) steps instead of the QMC algorithm*)

(** Turns on or off the output of debug messages.
  This is turned off by default. *)
val set_verbose_mode : bool -> unit

(** Sets whether the optimization results should be verfied.
  This is done by testing of the equivalence of the original
  expression and the minimized one. This is turned off by default.*)
val set_result_verification : bool -> unit

(** Optimizes the given expression with the specified algorithm. *)
val optimize : optimization_algorithm -> dnf_expression -> dnf_expression

(** Optimizes the given expression with the suitable algoirthm.
  When there is more then 12 non essential prime implicants
  Qmc_SimpleHeuristic_Best is used, otherwise Qmc_PetricksMethod.
  The result is a tuple of the minimized expression and a boolean
  which indicates whether the optimization was exact (true) or not. *)
val auto_optimize : dnf_expression -> (dnf_expression * bool)

(** Converts the given string to a minterm. '1' is true,
  '0' is false and 'x' is don't matter. *)
val dnf_minterm_of_string : string -> dnf_minterm

(** Converts the given string, separated  by the given separator to
  a dnf expression. '1' is true, '0' is false and 'x'
  is don't matter. *)
val dnf_expression_of_string: char -> string -> dnf_expression

(** Converts the given string list to a dnf expression.
  '1' is true, '0' is false and 'x' is don't matter.*)
val dnf_expression_of_string_list: string list -> dnf_expression

(** Loads a dnf expression from file with the given name. The file should
  contain one minterm per line. The format of the minterm
  representations is as following: '1' means true, '0' means false and
  'x' means don't matter. Lines starting with any other
  characters then '0', '1' or 'x' are ignored. *)
val load_from_file: string -> dnf_expression

(** Converts a dnf expression to a string. *)
val string_of_dnf_expression : dnf_expression -> string

(** Converts a minterm of a dnf expression to a string. *)
val string_of_dnf_minterm : dnf_minterm -> string

(** Prints a dnf expression to the standard output. *)
val print_dnf_expression : dnf_expression -> unit

(** Prints a minterm of a dnf expression to the standard
  output. *)
val print_dnf_minterm : dnf_minterm -> unit
