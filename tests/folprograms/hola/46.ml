let rec loop w z x y = 
	if (Random.bool ()) then
		let x = if (w mod 2 = 1) then x+1 else x in	
		let w = if (w mod 2 = 1) then w+1 else w in	
		let y = if (z mod 2 = 0) then y+1 else y in
		let z = if (z mod 2 = 0) then z+1 else z in
		loop w z x y
	else assert (x <= 1)

let main () = 
	let w = 1 in
	let z = 0 in
	let x = 0 in 
	let y = 0 in
	loop w z x y
	
let _ = main ()
let _ = main ()
let _ = main ()
let _ = main ()
let _ = main ()
