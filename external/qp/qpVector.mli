
type 'a vector
val make : int -> 'a -> 'a vector
val set : 'a vector -> int -> 'a -> unit
val get : 'a vector -> int -> 'a
val length : 'a vector -> int
val push : 'a vector -> 'a -> unit
val pop : 'a vector -> unit
val resize : 'a vector -> int -> unit
val back : 'a vector -> 'a
val print_stats : unit -> unit
