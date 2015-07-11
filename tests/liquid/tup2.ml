(* squalif ITV(x): x.1 <= x.2 *)

let zz = (1,2)

let _ = (fun x -> x) zz

let rec read_pos_int () = 
  let x = read_int () in
  if x >= 0 then x else read_pos_int ()


let _ = read_pos_int ()

let mmin (x:int) y = 
  if x <= y then x else y

let mmax (x:int) y = 
  if x <= y then y else x

let read_pair_wf () = 
  let x = read_int () in
  (x,x+1)

let rec mklist n = 
  if n < 0 then [] else 
    let p = read_pair_wf () in
    let xs = mklist (n-1) in
    p::xs

let lub (p: int*int) (p':int*int) = 
  let (x,y) = p in
  let (x',y') = p' in
  let x'' = mmin x x' in
  let y'' = mmax y y' in
  (x'',y'')

let _ = 
  let ys = mklist 100 in
  let b = (0,0) in
  let (x,y) = List.fold_left lub b ys in
  assert (x <= y) 

