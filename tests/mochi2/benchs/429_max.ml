(*
USED: PLDI2011 as max
USED: PEPM2013 as max
*)
let max (x:int) (y:int) (z:int) max2 : int = max2 (max2 x y) z
let f x y : int = if x >= y then x else y
let main (x:int) y z =
  let m = max x y z f in
  assert (f x m = m)

let _ = main (-1) (-2) (3)
let _ = main (3) (-1) (-2)
let _ = main (-2) (3) (-1)
