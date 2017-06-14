(*
USED: PLDI2011 as intro3
*)
let f x g :unit= g(x+1)
let h (z:int) y = assert (y>z)
let main n = if n>=0 then f n (h n) else ()

let _ = main 3
let _ = main (-2)
