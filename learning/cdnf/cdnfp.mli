(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
*****************************************************************************)


type var_t = int

type lit_t

type boolformula_type_t = Lit | And | Or

type boolformula_t

val var_of_literal : lit_t -> var_t

val literal_of_var : var_t -> lit_t

val literal_complement : lit_t -> lit_t

val positive_literal : lit_t -> bool

val from_literal : lit_t -> boolformula_t

val create_disjunction : int -> boolformula_t

val create_conjunction : int -> boolformula_t

val add_boolformula : boolformula_t -> boolformula_t -> boolformula_t

val set_boolformula : boolformula_t -> int -> boolformula_t -> boolformula_t

val get_type : boolformula_t -> boolformula_type_t

val get_length : boolformula_t -> int

val get_boolformula : boolformula_t -> int -> boolformula_t

val get_literal : boolformula_t -> lit_t

val print : boolformula_t -> unit

val num_vars : boolformula_t -> int

val to_cnf : boolformula_t -> int -> boolformula_t

val copy : boolformula_t -> boolformula_t

type membership_t = bool array -> Query.mem_result_t
type comembership_t = membership_t

type equivalence_t = int -> boolformula_t -> Query.eq_result_t

val go : int -> membership_t -> equivalence_t 
  -> boolformula_t option

val gox : int -> membership_t -> equivalence_t 
  -> boolformula_t

val goxx : int -> membership_t -> comembership_t -> equivalence_t 
  -> boolformula_t

val goxxx : int -> membership_t -> equivalence_t 
  -> boolformula_t

