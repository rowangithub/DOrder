let rec loopb w z x y= 
	if (Random.bool ()) then
		let x = if (w mod 2 = 1) then x + 1 else x in
		let y = if (z mod 2 = 0) then y + 1 else y in
		let z = x + y in
		let w = z + 1 in
		loopb w z x y
	else (x, y) 

let rec loop w z x y = 
	if (Random.bool ()) then
		let (x, y) = loopb w z x y in
		let z = x + y in
		let w = z + 1 in
		loop w z x y
	else assert (x = y)

let main () = 
	let (w, z, x, y) = (1, 0, 0, 0) in
	loop w z x y

let _ = main ()
let _ = main ()
let _ = main ()
let _ = main ()
let _ = main ()