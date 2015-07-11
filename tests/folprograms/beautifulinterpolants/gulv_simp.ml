let rec loop x y =
	if (Random.bool ()) then
		if (Random.bool ()) then loop (x+1) (y+100)
		else if (Random.bool ()) then
			if (x >= 4) then loop (x+1) (y+1)
			else loop x y
		else loop x y
	else assert (x < 4 || y > 2)

let main () = 
	let x = 0 in
	let y = 0 in
	loop x y	

let _ = main ()
let _ = main ()
let _ = main ()  