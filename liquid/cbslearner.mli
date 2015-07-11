(** This is the proposed constraints based learner
 * learn takes postive/negative samples and a vector of predicates as input and
 * returns candidate invariant
val learn: (Path.t, int) list -> (Path.t) list -> string list -> Predicate.t*)

(** the total size of hypothesis domain *)
val nb_hypo : int ref

val counter : int ref

type learning_result =
{ ipre : (bool * (string, Path.t) Hashtbl.t * (Predicate.t list * Predicate.t)) list;
ipost : (bool * (string, Path.t) Hashtbl.t * (Predicate.t list * Predicate.t)) list}

val learn : int -> (Predicate.binrel * int) list -> 
(Path.t, Types.type_declaration ) Hashtbl.t ->
(string, Predicate.t list) Hashtbl.t ->
(string, string * (string list * string list) list) Hashtbl.t ->
(Path.t, Modelsolver.solving_result) Hashtbl.t ->
(Path.t, learning_result) Hashtbl.t -> 
Backwalker.function_env -> Env.t -> unit

val gen_inv : (string, Predicate.t list) Hashtbl.t ->
(string, string * (string list * string list) list) Hashtbl.t ->
(Path.t, Modelsolver.solving_result) Hashtbl.t ->
(Path.t, learning_result) Hashtbl.t -> 
Backwalker.function_env -> Env.t -> unit