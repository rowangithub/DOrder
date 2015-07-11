qualif POS(x): 0 < x
qualif NNEG(x): 0 <= x
qualif NEG(x): x < 0

let rec mymap f lst = match lst with
  | [] -> []
  | x :: xs -> (f x) :: mymap f xs

let _ = mymap

let abs x = if x < 0 then 0 - x else x

let l = [0; 1; -2; 3]

let k = mymap abs l

let _ = (fun z -> z) k
