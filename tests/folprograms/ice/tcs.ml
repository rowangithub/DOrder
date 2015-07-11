let rec loop (i:int) (j:int) x y = 
	if (x = 0) then (
		if (i = j) then
		assert (y = 0)
		else ()		
	) else
	loop i j (x-1) (y-1)
		
let main i j =
	loop i j i j


let _ = main 3 3
let _ = main 2 2
let _ = main 2 5
let _ = main 5 3
