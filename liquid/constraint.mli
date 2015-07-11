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

type fc_id 

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * guard_t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of labeled_constraint

val qual_test_var : Path.t
val qual_test_expr : Predicate.pexpr

val fresh_fc_id : unit -> fc_id 

val solve : Path.t list -> (Path.t -> Frame.t -> bool) -> (unit -> Qualifier.t list) ->
	(('a, 'b) Hashtbl.t -> ('c, 'd) Hashtbl.t) ->
  (('a, 'b) Hashtbl.t -> ('c, 'd) Hashtbl.t -> ('e, Modelsolver.solving_result) Hashtbl.t) ->
	((string, Predicate.t list) Hashtbl.t ->
		('c, 'd) Hashtbl.t ->
			('e, Modelsolver.solving_result) Hashtbl.t ->
				('a, 'b) Hashtbl.t * (Path.t option * Qualifier.t) list) ->
	(('f, 'g) Hashtbl.t ->
    ('c, 'd) Hashtbl.t ->
    	('e, Modelsolver.solving_result) Hashtbl.t ->
    		('a, 'b) Hashtbl.t * (Path.t option * Qualifier.t) list) ->
  Qualifier.t list -> labeled_constraint list -> (Path.t -> Qualifier.t list) * labeled_constraint list * int


