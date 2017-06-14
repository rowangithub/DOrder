let g (x:int) = 2 * x

let twice (x:int) f = 
	f 
	(f x)

let neg x = 
	(0 - x)

let main n =
	let z = twice (g n) neg in
  if (n > 0) then assert (z>=0)
	else assert (z <= 0)
	
	
let _ = main 1
let _ = main (-1)
