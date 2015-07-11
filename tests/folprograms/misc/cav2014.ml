let rec loop i j p = 
	if i < 100 then
		if (i >= p) then loop (i+1) 1 p
		else loop (i+1) j p
	else assert (j = 1)

let main p = 
	let i = 0 in
	let j = 0 in
	if (p >= 25 && p < 75) then
		loop i j p
	else ()
	
let _ = main 50
let _ = main 25
let _ = main (-1)