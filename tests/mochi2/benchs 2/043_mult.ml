let rec mult n m =
  if n <= 0 then 0
	else if m <= 0 then 0
  else
    n + mult n (m-1)
		
let main n = assert (n <= mult n n)

let _ = main 3
let _ = main 2
let _ = main (-2)
