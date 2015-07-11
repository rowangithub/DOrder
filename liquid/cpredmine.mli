(* Mine interesting predciates from test data *)

(*fun synthesize flag pos_samples dranges tbl enforces*)
val synthesize : int -> Path.t -> (string * int) list list -> 
	int list -> int list -> (string, Path.t) Hashtbl.t -> string list -> (Predicate.t) list 
	
val synthesize_octagon : Path.t -> (string * int) list list -> 
	int list -> int list -> (string, Path.t) Hashtbl.t -> string list -> (Predicate.t) list 
	
val synthesize_datatype_constraints	: Path.t -> (string * int) list list -> 
	int list -> int list -> (string, Path.t) Hashtbl.t -> string list -> (Predicate.t) list 