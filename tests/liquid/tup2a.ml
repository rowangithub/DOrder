let mmin (x:int) y = 
  if x <= y then x else y

let mmax (x:int) y = 
  if x <= y then y else x

let rec mklist n = 
  if n < 0 then [] else 
    let xs = mklist (n-1) in
    n::xs

let f x = 
  let x' = x + 1 in
  (x,x')

let g (p: int*int) (p':int*int) = 
  let (x,y) = p in
  let (x',y') = p' in
  let x'' = mmin x x' in
  let y'' = mmin y y' in
  (x'',y'')


let _ = 
  let ys = mklist 100 in
  let ys' = List.map f ys in
  let b = (0,0) in
  let (x,y) = List.fold_left g b ys' in
  assert (x <= y) (* shockingly, this is safe!*)

