let rec loop x y z w = 
	let w = w + 1 in
	let z = z + 10 in
	if (Random.int 100 > 4) then (
		if (Random.int 100 > 50) then loop (x+1) (y+100) z w 
		else if (Random.int 100 > 50) then
			if (x >= 4) then loop (x+1) (y+1) z w else loop x y z w
		else if (y > 10*w && z >= 100*x) then loop x (-y) z w
		else loop x y z w
	)
	else assert (x < 4 || y > 2)

let main () = 
	let x = 0 in
	let y = 0 in
	let z = 0 in
	let w = 0 in
	loop x y z w
	
let _ = main ()
let _ = main ()