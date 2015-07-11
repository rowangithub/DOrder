let rec ackermann m n =
  if m=0 then
    n+1
  else if n=0 then
    ackermann (m-1) 1
  else
    ackermann (m-1) (ackermann m (n-1))
		
let main m n = if (m>=0 && n>=0) then assert (ackermann m n >= n) else ()

let _ = main 2 2
let _ = main 0 0
let _ = main (-1) (-1)