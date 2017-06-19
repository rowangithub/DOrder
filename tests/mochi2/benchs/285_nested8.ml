let rec loopc i j k n m = 
	if k < n + m then
		(assert (i + j <= n + k + m);
		loopc i j (k+1) n m)
	else ()	
	
let rec loopb i j (n:int) (m:int)	=
	if j < n then
		(loopc i j j n m;
		loopb i (j+1) n m)
	else ()
		
let rec loopa i (n:int) (m:int) = 
	if i < n then
		(loopb i 0 n m;
		loopa (i+1) n m)
	else ()
	
let main n m =
	if n <= m then 
		loopa 0 n m
	else ()