(* For learning algorithm 1 *)

val learn : int -> (string * int) list list -> (string * int) list list -> (string, Path.t) Hashtbl.t ->
	string list -> Env.t -> Frame.t -> ((*Predicate.t list *) Predicate.t) list 
	

	