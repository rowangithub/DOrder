let rec loop (i:int) (j:int) x y = 
	if (x = 0) then (
		if (i = j) then
		assert (y = 0)
		else ()		
	) else
	loop i j (x-1) (y-1)
		
let main i j =
	loop i j i j