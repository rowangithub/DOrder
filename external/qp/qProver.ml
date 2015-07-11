module Misc = QpMisc
module Eq = QpEquality.Prover
module Dc = QpDiff.Prover
open QpDag

type fact = EqFact of predicate | DcFact of predicate

let nb_instance = ref 0

type instance = Eq.instance

let is_atom = function
  | Equality (_,_),_ | Not (Equality (_,_),_),_ 
  | Leq (_,_),_ | Not (Leq (_,_),_),_ 
  | True,_ | False,_ -> true
  | _ -> false

let rec atoms p = 
  match p with 
  | _ when is_atom p -> [p]
  | And ps,_ -> Misc.flap atoms ps  
  | _ -> [] 

let grind eq dc q = 
  while not (Queue.is_empty q) do
    (match Queue.pop q with 
    | EqFact p -> Dc.push dc p
    | DcFact p -> Eq.push eq p);
  done

let push eq dc p =
  assert (is_atom p);
  Eq.push eq p; 
  Dc.push dc p

let is_valid eq dc q = 
  if is_atom q then (Eq.is_valid eq q || Dc.is_valid dc q) else false

let check_imp p =
  let pending = Queue.create () in
  let _ = incr nb_instance in
  let eq_inst = Eq.new_instance (fun p -> Queue.push (EqFact p) pending) in   
  let dc_inst = Dc.new_instance (fun p -> Queue.push (DcFact p) pending) in   
  let _ = List.iter (push eq_inst dc_inst) (atoms p) in
  let _ = grind eq_inst dc_inst pending in
  fun q -> is_valid eq_inst dc_inst q

let check_imps p = 
  List.map (check_imp p)

let print_stats () = 
  Printf.printf "QProver Instances = %d \n" !nb_instance

(* {{{
let check_imps_dag p qs =
  let me = Eq.new_instance (fun _ _ _ -> ()) in   
  List.iter (push me) (atoms p);
  List.map (is_valid me) qs

let check_imps p qs = 
  check_imps_dag (pred_dag_of_tree p) (List.map pred_dag_of_tree qs) 
}}} *)


