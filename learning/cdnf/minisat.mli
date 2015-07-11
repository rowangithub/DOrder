
type var_t
type lit_t
type solver_t

(* the value of a variable in an assignment *)
type value_t = FALSE | TRUE | UNKNOWN

val new_solver : unit -> solver_t
val new_var : solver_t -> var_t
val new_inferred_var : solver_t -> var_t
val pos_lit : solver_t -> var_t -> lit_t
val neg_lit : solver_t -> var_t -> lit_t
val add_clause : solver_t -> lit_t array -> unit
val simplify : solver_t -> unit
val solve : solver_t -> bool
val solve_with_assumption : solver_t -> lit_t array -> bool
val value_of : solver_t -> var_t -> value_t
val nvars : solver_t -> int
val okay : solver_t -> bool
val get_witness : solver_t -> var_t -> bool
