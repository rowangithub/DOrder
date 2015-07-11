let rec loop i n a b =
	if (i < n) then
		if (Random.bool ()) then
			loop (i+1) n (a+1) (b+2)
		else loop (i+1) n (a+2) (b+1)
	else assert (a+b = 3 * n)

let main n = 
	if (n >= 0) then
		let (i, a, b) = (0, 0, 0) in
		loop i n a b
	else ()
	
let _ = main 5
let _ = main (-2)