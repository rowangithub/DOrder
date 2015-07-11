
(** check_imp p q = valid(p => q) *) 
val check_imp: QpDag.predicate -> (QpDag.predicate -> bool)

(** check_imps p [q1;...;qn] = [b1;...;bn] where bi <=> valid(p => qi) *) 
val check_imps: QpDag.predicate -> QpDag.predicate list -> bool list

val print_stats: unit -> unit
