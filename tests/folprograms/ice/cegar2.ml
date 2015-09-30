let rec loop x m n = 
	if (x < n) then
		if (Random.bool ()) then 
			loop (x+1) x n
		else loop (x+1) m n 
	else m
	
	(*else if (n > 0) then assert (0 <= m && m < n)
	else () *)

let main n = 
	let x = 0 in
	let m = 0 in
	let m = loop x m n in
	if (n > 0) then
		assert (0 <= m && m < n)
	else ()
	
let _ = main (-5)	