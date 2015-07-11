exception MINEFAIL

val naive_mine_assoc:
(int list list) -> Predicate.t list -> Env.t -> Frame.t -> (Predicate.t * float * int) list

val manual_mine_assoc: 
(int list list) -> Predicate.t list -> Env.t -> Frame.t -> (string, Path.t) Hashtbl.t -> (Predicate.t) list

val mine_template: bool ->
(string list * string list) list -> string list -> Env.t -> Frame.t -> Predicate.t -> Predicate.t -> (Path.t list * Predicate.t list)

val normalize:
Predicate.t -> Path.t -> Predicate.t
