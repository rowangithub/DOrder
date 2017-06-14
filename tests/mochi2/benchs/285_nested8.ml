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
		

	
let _ = main 2 2
let _ = main 2 3
let _ = main 3 2
let _ = main 1 1
let _ = main 1 0
let _ = main 0 1
let _ = main 0 2
let _ = main 2 0 
let _ = main (-2) (-2)