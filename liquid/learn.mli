val learn :
           (string, string * (string list * string list) list) Hashtbl.t ->
           (Path.t, (String.t * int) list list) Hashtbl.t ->
           (Path.t, Predicate.t list) Hashtbl.t -> unit
					
					