(*
USED: PLDI2011 as ack
USED: PEPM2013 as ack
*)

let rec ack m n =
  if m=0 then n+1
  else if n=0 then ack (m-1) 1
  else ack (m-1) (ack m (n-1))

let main m n =
  if (m>=0 && n>=0)
  then assert (ack m n >= n)
  else ()

let _ = main 0 0
let _ = main 3 2
let _ = main (-2) (-2)	
