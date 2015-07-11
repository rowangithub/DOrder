(* let f n = ref n 
let decr r = r := !r - 1
let incr r = r := !r + 1

let x = f 0
let _ = (fun y -> y) !x
let a = !x 
let x1 = x
(* let _ = decr x1 (* DECRX1 *) *) 
let b = !x 
let xr = f 1
let xr' =  f 10
let _ = decr xr' 
(* let _ = incr xr' (* INCRXR' *) *)
let _ = incr xr

let _ = assert (!xr >= 0) (* OK but fails if INCRXR' *)

(* 
let _ = assert (a >= 0) (* OK but fails if DECRX1 *)
let _ = assert (b >= 0)   (* FAIL *)
let _ = assert (!xr'>= 0) (* FAIL *)
*)
*)

(*************************************************************************)
(* 
type mysum = N of int * int | I of int 

let g z = 
  match z with
  | I i -> () (* assert (i >= 0) *)
  | N (i,j) -> ()

*)

type node = { mutable data : int; 
              mutable next : node option }

let mk_node x no = { data = x; next = no }

(*
let rec list_to_node xs = 
  match xs with 
  | [x]    -> mk_node x None
  | x::xs' -> mk_node x (Some (list_to_node xs'))
  | _      -> assert (false)

let rec node_to_list n = 
  n.data :: (match n.next with Some n' -> node_to_list n' | None -> [])

let n0 = list_to_node [1;2;3]
let n1 = list_to_node [-1;-2;-3]

let xs0 = node_to_list n0
let _ = List.iter (fun x -> assert (x >= 0)) xs0
*)

