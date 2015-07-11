let rec loop w z x y =
	if (Random.int 10 > 1) then
		let (x, w) = 
			if (w > 0) then (x+1, 1-w)
			else (x, w) in
		let (y, z) = 
			if not (z > 0) then (y+1, 1-z)
			else (y, z) in
		loop w z x y
	else assert (x=y)
	

let main () = 
	let (w, z, x, y) = (2, (-1), 0, 0) in
	loop w z x y
	
let _ = main ()