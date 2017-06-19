let rec loopc i j k n = 
	if k < j then
		(assert (k - i <= 2 * n);
		loopc i j (k+1) n)
	else ()	
	
let rec loopb i j n = 
	if j < 3 * i then
		(loopc i j i n;
		loopb i (j+1) n)
	else ()	

let rec loopa i n = 
	if i < n then 
		(loopb i (2*i) n;
		loopa (i+1) n) 
	else ()
	

let main n =
	loopa 0 n