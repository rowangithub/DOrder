let p = (1,2)

let _ = (fun x -> x) p

let f x = 
  let x' = x + 1 in
  (x,x')

let g xs = 
  List.map f xs

let rec mklist n = 
  if n < 0 then [] else 
    let xs = mklist (n-1) in
    n::xs

let ys = mklist 100

let ys' = g ys

let _ = List.iter (fun z -> let ((x:int),y) = z in assert (x <= y)) ys'
