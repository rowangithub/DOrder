let rec loop x y =
	if (x < 0) then
		loop (x+y) (y+1)
	else assert (y > 0)
	
let main y  = 
	let x = -50 in
	loop x y
	

let _ = main (-5)