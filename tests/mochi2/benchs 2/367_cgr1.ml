let rec loop x y = 
	if (x < 100) then
		loop (x+2) (y+2)
	else assert (x <> 4 || y <> 0)

let main x y = 
	if (0 <= x && x <= 2 && 0 <= y && y <= 2) then
		loop x y
	else () 
	

let _ = main (-1) (-1)