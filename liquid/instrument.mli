
val log_fname : string

val tempt_arr_prefix : string
val tempt_arr_postfix : string

val print_min_max : Format.formatter -> unit

val print_harnesses : string -> Format.formatter -> unit

val print_udt : string list -> (Path.t, Types.type_declaration) Hashtbl.t -> Format.formatter -> unit

val dump_side_effects : Format.formatter -> Backwalker.function_env -> 
								(Location.t -> Frame.t) -> Location.t -> 
								string -> Env.t -> Frame.t Lightenv.t -> unit

val dump_fun : Format.formatter -> Backwalker.function_env -> 
								(Location.t -> Frame.t) -> Location.t -> 
								string -> Env.t -> Frame.t Lightenv.t -> Parsetree.expression -> unit
								
val dump_app : Format.formatter -> Backwalker.function_env -> Location.t -> Parsetree.expression -> unit								
								
(** At a particular location, a function is called with certain parameters and certain environment  *)								
val read_dumpings : (string, string * (string list * string list) list) Hashtbl.t -> unit