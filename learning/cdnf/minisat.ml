
type var_t = int
type lit_t = int
type solver_t

(* the value of a variable in an assignment *)
type value_t = FALSE | TRUE | UNKNOWN

external new_solver : unit -> solver_t = "minisat2_new_solver"
external new_var : solver_t -> var_t = "minisat2_new_var"
external new_inferred_var : solver_t -> var_t = "minisat2_new_inferred_var"
external pos_lit : solver_t -> var_t -> lit_t = "minisat2_pos_lit"
external neg_lit : solver_t -> var_t -> lit_t = "minisat2_neg_lit"
external add_clause : solver_t -> lit_t array -> unit = 
    "minisat2_add_clause"
external simplify : solver_t -> unit = "minisat2_simplify"
external solve : solver_t -> bool = "minisat2_solve"
external solve_with_assumption : solver_t -> lit_t array -> bool = 
    "minisat2_solve_with_assumption"
external value_of : solver_t -> var_t -> value_t = "minisat2_value_of"
external nvars : solver_t -> int = "minisat2_nvars"
external okay : solver_t -> bool = "minisat2_okay"

(**
   Get the satisfiable assignment.
*)
let get_witness solver var =
  let value = value_of solver var in
    match value with
	FALSE -> false
      | TRUE -> true
      | UNKNOWN -> false (* arbitrary value *)
