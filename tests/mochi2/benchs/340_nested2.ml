let rec loopc i k n = 
	if i < n then
		(assert (k >= 0);
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
	loopa 0 l n