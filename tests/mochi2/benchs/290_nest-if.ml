let rec loopb i k n = 
	if i < n then 
		(assert (1 <= k);
		loopb (i+1) k n)
	else ()
	
let rec loopc i n = 
	if i < n then 
		loopc (i+1) n
	else () 

let rec loopa i k n = 
	if k < n then
		(loopb 1 k n;
		if i < n then 
			loopc 1 n
		else ();
		loopa i (k+1) n)
	else ()

let main i n = 
	let k = 1 in
	loopa i k n