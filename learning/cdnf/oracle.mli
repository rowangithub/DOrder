(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************)

type result_t = bool array option

val is_satisfiable_with_assumption : int -> BoolFormula.t -> bool array 
  -> result_t

val is_equivalent : int -> BoolFormula.t -> BoolFormula.t -> result_t

val is_statisfiable : int -> BoolFormula.t -> result_t

