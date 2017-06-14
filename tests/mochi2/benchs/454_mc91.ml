(*
USED: PLDI2011 as mc91
USED: PEPM2013 as mc91
*)

let rec mc91 x =
  if x > 100 then
    x - 10
  else
    mc91 (mc91 (x + 11))
let main n =
  if n <= 101 then assert (mc91 n = 91) else ()

let _ = main 102
let _ = main (-2)
