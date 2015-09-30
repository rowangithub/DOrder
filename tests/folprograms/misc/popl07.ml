let rec loop x y = 
	if (x < 100) then 
		if (x < 50) then
			loop (x+1) y
		else loop (x+1) (y+1)
	else y



let main () = 
	let x = 0 in
	let y = 50 in
	let res = loop x y in
	assert (res = 100)
	
let _ = main ()
	
