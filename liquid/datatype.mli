val forall_uvar : Path.t
val forall_uexpr : Predicate.pexpr
val forall_vvar : Path.t
val forall_vexpr : Predicate.pexpr
val foralls : Path.t list

val symbexe_measure : Backwalker.function_env -> Env.t -> unit

val update_measure : Backwalker.function_env -> (Path.t, Types.type_declaration) Hashtbl.t ->
											Constraint.labeled_constraint list -> Constraint.labeled_constraint list
val extract_value_type_from_container_fr : Frame.t -> (bool * Path.t)											
val extract_value_type_from_container_ty : Types.type_desc -> (bool * Path.t)											
val get_all_links : (Path.t, Types.type_declaration) Hashtbl.t -> Path.t -> (string * int) list