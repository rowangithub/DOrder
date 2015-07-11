type inst
type id = int
val new_instance: unit -> inst   
val set_merge_callback: inst -> (id -> id -> unit) -> unit
val new_atom: inst -> id 
val new_application: inst -> string -> (id list) -> int 
val check_equal: inst -> id -> id -> bool
val assert_equal: inst -> id -> id -> unit 
