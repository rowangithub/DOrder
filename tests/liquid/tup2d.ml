let mmin (x:int) y = 
  if x <= y then x else y

let mmax (x:int) y = 
  if x <= y then y else x

(* 
let read_pair_wf () = 
  let x = read_int () in
  (x,x+1)
*)

let rec mklist n = 
  if n < 0 then [] else 
    let p = (n,n+1) in 
    let xs = mklist (n-1) in
    p::xs

let g (p: int*int) (p':int*int) = 
  let (x,y) = p in
  let (x',y') = p' in
  let x'' = mmax x x' in
  let y'' = mmin y y' in
  (x'',y'')


let _ = 
  let ys = mklist 100 in
  let b = (10,10) in
  let (x,y) = List.fold_left g b ys in
  assert (x <= y) (* fails *)
