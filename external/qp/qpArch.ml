module type PROVER = 
  sig
    type instance
    val new_instance:       (QpDag.predicate -> unit) -> instance 
    val push:               instance -> QpDag.predicate -> unit
    val is_consistent:      instance -> bool
    val is_valid:           instance -> QpDag.predicate -> bool
  end
