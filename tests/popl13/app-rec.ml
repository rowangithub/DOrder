let apply (b:int) (f:int->unit) x = f x
let check (x:int) (y:int) = assert (x = y)
let rec loop n = 
	if Random.bool () then (apply n (check n) n; loop (n + 1)) else ()
let main (x:int) = loop x

let _ = main 2