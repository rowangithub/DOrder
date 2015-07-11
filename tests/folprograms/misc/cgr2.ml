let rec loop (n:int) (m:int) (x:int) = 
	if (x < n) then
		if (Random.bool ()) then loop n x (x+1)
		else loop n m (x+1)
	else (
		if (n > 0) then assert (0 <= m && m < n)
		else ()
		)
	
let main n = 
	loop n 0 0 



let _ = main 3
let _ = main 4
let _ = main 0
let _ = main (-1)