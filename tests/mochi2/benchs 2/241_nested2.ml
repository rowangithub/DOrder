let rec loopc i k n = 
	if i < n then
		(assert (1 <= k);
		loopc (i+1) k n)
	else ()

let rec loopb i k n = 
	if i < n then
		loopb (i+1) k n
	else ()

let rec loopa k l n = 
	if k < n then
		(loopb l k n;
		loopc l k n;
		loopa (k+1) l n)
	else ()
	
let main l n = 
	loopa 1 l n
	
let _ = main 2 3
let _ = main (-2) (-3)