let rec loopc i j k n =
	if k < n then
		(assert (k >= 2 * i);	
		loopc i j (k+1) n)
	else ()

let rec loopb i j k n =
	if j < n then
		if (Random.bool ()) then
			(loopc i j j n;
			loopb i (j+1) k n)
		else (
			assert (k >= n);
			assert (k <= n);
			loopb i (j+1) k n)
	else ()	

let rec loopa i k n = 
	if i < n then
		(loopb i (2*i) k n;
		loopa (i+1) k n)
	else ()
	

let main k n =
  if (k = n) then
		loopa 0 k n
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
 