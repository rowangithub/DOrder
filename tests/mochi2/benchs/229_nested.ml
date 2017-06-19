let rec loopb i j n = 
	if j < n then
		(assert (0<= j && j <=n && 0 <= i && i <= n);
		loopb i (j+1) n)
	else ()

let rec loopa i n = 
	if i < n then
		(loopb i 0 n;
		loopa (i+1) n)
	else ()
	
let main n = 
	if n <= 0 then ()
	else 
		loopa 0 n