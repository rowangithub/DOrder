let rec loop i sn = 
	if (i <= 8) then loop (i + 1) (sn + 1) 
	else (
		if (sn = 8) then ()
		else assert (sn = 0)	
	)
	
let main () = 
	loop 1 0
	
let _ = main ()	