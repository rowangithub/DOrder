let f (x:int) = x

let main x = 
	assert (f x >= x)
	
let _ = main 0
let _ = main (-2)	