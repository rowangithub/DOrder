let rec loop x y z w =
	if (Random.int 100 > 4) then 
  	if (Random.int 100 > 50) then loop (x+1) (y+2) z w
		else if (Random.int 100 > 50) then 
			if (x >= 4) then loop (x+1) (y+3) (z+10) (w+10)
			else loop x y z w
		else if (x >= z && w >= y+1) then loop (-x) (-y) z w
		else loop x y z w	
	else 
		assert (3*x > y-1)

let main () =
	let x, y, z, w = 0, 0, 0, 0 in
  loop x y z w



    
let _ = main ()
let _ = main ()
let _ = main ()