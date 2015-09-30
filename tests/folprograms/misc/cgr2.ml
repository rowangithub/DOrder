let rec loop (n:int) (m:int) (x:int) = 
	if (x < n) then
		if (Random.bool ()) then loop n x (x+1)
		else loop n m (x+1)
	else m
	
let main n = 
	let m = loop n 0 0 in
	if (n > 0) then 
		assert (0 <= m && m < n)
	else ()



let _ = main (-1)