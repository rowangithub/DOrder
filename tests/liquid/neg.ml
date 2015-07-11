let g (x:int) = 2 * x

let twice (x:int) f = 
	f 
	(f x)

let neg x = 
	(0 - x)

let main n =
  if n>=0 then
    let z = twice (g n) neg in
    assert (z>=0)
	else ()
	
let _ = main 1
let _ = main (-1)