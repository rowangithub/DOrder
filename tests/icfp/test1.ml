let rec loop x y z lock = 
	if (Random.bool ()) then
		if (x <> y) then
			let lock = 0 in
			let x = y in
			if (0 <= z) then 
				loop x (y+1) z 1
			else loop x y z lock
		else 
			assert (lock = 0)
	else ()

let main x z = 
	let lock, y = 
		if (z >= 0) then 1, x+1
		else 0, x in
	loop x y z lock
	
let _ = main 2 3
let _ = main (-2) (-3)	