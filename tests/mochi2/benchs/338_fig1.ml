let rec loop x y =
	if (x < 0) then
		loop (x+y) (y+1)
	else y
	
let main y  = 
	let x = -50 in
	let result = loop x y in
	assert (result > 0)
	
let _ = main 0
let _ = main (-100)