qualif NNEG(v): v >= 0 
qualif LT_n(v): v < n
qualif LT_len_a(v): v < Array.length a

let abs x = 
  if x > 0 then x else (0 - x) 

let foldn n b f =
  let rec loop i c =
    if i < n then loop (i+1) (f i c) else c in
    loop 0 b

let arraysum a =
  let am l s = s + (abs (Array.get a l)) in
    foldn (Array.length a) 0 am

let _ = 
  let vec = Array.make (Random.int 40) (-1) in
  arraysum vec

