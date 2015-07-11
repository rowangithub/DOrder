let rec loop x y z =
	if (x < 100) then
		let y = if (x <= 50) then y+1 else y-1 in
		let z = if (x < 25) then z+1 else z+5 in
		loop (x+1) y z
	else (
		assert (z = 400);
		assert (y = 2);
		assert (x = 100)
		)

let main () = 
	let x = 0 in
	let y = 0 in
	let z = 0 in
	loop x y z
	
let _ = main ()