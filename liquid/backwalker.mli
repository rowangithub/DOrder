(*exception Datatype*)

val main_function : string ref
val main_function_path : Path.t ref
val curr_function_path : Path.t ref

val qual_test_var : Path.t
val qual_test_expr : Predicate.pexpr

val data_structure_dealing_flag : bool ref
val assertEqual : bool ref

val hoflag : bool ref

val tests : int ref

type function_const = {pre: Predicate.t; post: Predicate.t}
type function_env = {
	(** Scan the structure to find all function bindings *)
	funbindings: (Path.t, (bool * Typedtree.expression)) Hashtbl.t;
	funframebindings: (Path.t, Frame.t) Hashtbl.t;
	(** Scan the log to find all higher order function instantiations *)
	(*hobindings: (Path.t, Path.t) Hashtbl.t; *)
	(** Store the results of bad constraints *)
	badbindings : (Path.t, function_const) Hashtbl.t;
	(** Store function effect (very underapproximate) *)
	effectbindings : (Path.t, Predicate.t) Hashtbl.t;
	(** Strore funcion rturn (very simple for local use only) *)
	returnbindings : (Path.t, Predicate.t) Hashtbl.t;
	(** Store function assertion *)
  assertbindings : (Path.t, Predicate.t) Hashtbl.t;
	(** Find frame from log *)
	frames: (Typedtree.expression -> Frame.t);
	builtin_funs : Path.t list;
	funcalls : (string list) ref;
	funcallenvs: (Location.t, (Path.t * Frame.t) list) Hashtbl.t;
	fundefenvs : (Path.t, (Path.t * Frame.t) list) Hashtbl.t;
	(* record how each higher order function parameter is mapped to a concrete function *)
	hobindings: (Path.t, Predicate.pexpr) Hashtbl.t;
	(* Indicating if commons should be used for guarding the inferred refinements *)
	partial_used : bool ref;
	(* Indicating if data type is defined in the source program *)
	dty : bool ref;
	(* If dty is set, there are possibly measures defined for the data type *)
	measures : (Path.t, (Path.t * bool) list) Hashtbl.t;
	(* Measure's definition; key : type + constructor; value : measure + args + def *)
	measure_defs : ((Path.t * string), (string * Path.t list * Predicate.t) list) Hashtbl.t
	}

(*val forward_scan_structure : function_env -> Typedtree.structure -> unit *)

val symb_exe_structure : function_env -> Typedtree.structure -> unit

val symb_exe : (bool * Path.t list) -> function_env -> Typedtree.expression ->
	(Predicate.t * Predicate.t * Predicate.t * Predicate.t) -> (Predicate.t * Predicate.t * Predicate.t * Predicate.t)

val detect_arr_adj_pattern : 
	Predicate.t -> (Path.t * Frame.t) list -> (Predicate.pexpr) list -> 
	(Predicate.pexpr * Predicate.pexpr) list
	
val is_measure : function_env -> string -> bool	

val drive_new_test: Env.t -> function_env -> (Path.t, Predicate.t list  * Predicate.t list) Hashtbl.t ->
	Typedtree.structure_item list -> (bool * string list)
	
val gen_atomics : unit -> (Predicate.binrel * int) list
val get_constants : Path.t -> int list
val get_coeffs : Path.t -> int list
val no_need_splitter : Path.t -> bool
val find_atomics : Path.t -> bool -> (Predicate.t) list