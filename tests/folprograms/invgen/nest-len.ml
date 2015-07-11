let rec loopb i n = 
	if i < n then
		loopb (i+1) n
	else ()
	
let rec loopa k n = 
	(assert (1 <= k);
	loopb 1 n;
	loopb 1 n;
	loopb 1 n;
	loopb 1 n;
	loopb 1 n;
	loopb 1 n;
	loopb 1 n;)
	
let main n = 
	loopa 1 n
	
let _ = main 2
let _ = main (-2)