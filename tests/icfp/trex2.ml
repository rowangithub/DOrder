let foo x = x - 1

let rec loop x = 
	if x > 0 then
		let x = 
			if (Random.bool ()) then foo x
			else foo x in
		loop x
	else assert (x <= 0)

let main () =
	let x = if (Random.bool ()) then Random.int 10 else (0 - Random.int 10) in
	loop x
	
let _ = main ()