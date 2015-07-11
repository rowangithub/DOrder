(* squalif POS(x): 0 <= x
   squalif NEG(x): x < 0 *)

let rec fold f b xs =
  match xs with
  | [] -> b
  | x::xs' -> fold f (f b x) xs'

let rec map f xs =
  match xs with
  | [] -> []
  | x::xs' -> (f x)::(map f xs')

let abs x =
  if x > 0 then x else (0 - x)

let ys = [1;2;3;-4;-12]

let check_pos x = 
  assert(0 <= x)

let add x y =
  let t = abs y in
  x + t

let m = map abs ys

let _ = map check_pos m

let f = fold add 0 ys
let _ = assert(0 <= f)
