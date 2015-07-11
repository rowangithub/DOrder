let rec loop x y =
	if x <= 9 then
		loop (x+1) (y+1)
	else 
		assert (y >= 0)

let main () = 
	let x = 0 in
	let y = 0 in
	loop x y
	
let _ = main ()	