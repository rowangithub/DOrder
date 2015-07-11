let rec loop x y = 
	if (Random.int 100 > 1) then
		loop (x+2) (y+2)
	else assert (x <> 4 || y <> 0)

let main x y = 
	if (0 <= x && x <= 2 && 0 <= y && y <= 2) then
		loop x y
	else () 
	
let _ = main 0 2
let _ = main 2 0
let _ = main 4 0
let _ = main 0 4
let _ = main (-1) (-1)