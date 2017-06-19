let rec loop x y =
	if (x < 0) then
		loop (x+y) (y+1)
	else y
	
let main y  = 
	let x = -50 in
	let res = loop x y in
	assert (res > 0)