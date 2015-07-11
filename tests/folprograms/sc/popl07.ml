let rec loop x y = 
	if (x < 100) then 
		if (x < 50) then
			loop (x+1) y
		else loop (x+1) (y+1)
	else assert (y = 100)



let main () = 
	let x = 0 in
	let y = 50 in
	loop x y
	
let _ = main ()
	
