type solving_result = {
	spre : ((string * int) list) list; 
	spost : ((string * int) list) list 
}

val solve : 
	bool ->
 	Env.t ->
	(Path.t, Frame.t) Hashtbl.t -> 
	(Path.t, (Predicate.t list * Predicate.t list * Backwalker.function_const)) Hashtbl.t -> 
	(Path.t, Predicate.pexpr list) Hashtbl.t ->
	(Path.t, solving_result) Hashtbl.t -> 
	unit