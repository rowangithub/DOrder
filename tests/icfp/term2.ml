let rec loop x z = 
	if (x<100 && 100<z) then
		if (Random.bool ()) then loop (x+1) z
		else loop (x-1) (z-1)
	else assert (x >= 100 || z <= 100)

let main x y z = 
	
	if (x < 100 && z < 100) then loop x z
	else ()