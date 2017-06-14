(*
USED: PLDI2011 as intro1
*)
let f x g : unit= g(x+1)
let h y = assert (y>0)
let main n = if n>0 then f n h else ()

let _ = main 3
let _ = main (-2)