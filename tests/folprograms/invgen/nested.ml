let rec loopb i n = 
	if i < n then 
		loopb (i+1) n
	else ()

let rec loopa k n = 
	if k < n then
		(loopb 1 n;
		loopa (k+1) n)
	else assert (k >= 1)
	
let main n = 
	let k = 1 in
	loopa k n
	
let _ = main 2
let _ = main (-2)			