let rec loop (n:int) (m:int) (x:int) = 
	if (x < n) then
		if (x mod 2 = 0) then loop n x (x+1)
		else loop n m (x+1)
	else (
		m
		)
	
let main n = 
	let res = loop n 0 0 in
	if (n > 0) then assert (0 <= res && res < n)
	else ()


let _ = main 5
let _ = main 0
let _ = main (-5)