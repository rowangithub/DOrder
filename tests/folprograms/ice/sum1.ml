let rec loop i n sn = 
	if (i <= n) then loop (i + 1) n (sn + 1) 
	else (
		if (sn = n) then ()
		else assert (sn = 0)	
	)
	
let main n = 
	loop 1 n 0	
	
let _ = main 10
let _ = main 0
let _ = main (-1)