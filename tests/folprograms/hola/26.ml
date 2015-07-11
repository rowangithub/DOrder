let rec loopc w z x y= 
	if (Random.bool ()) then 
		let z = x + y in
		let w = z + 1 in
		loopc	w z x y
	else z, w
	
let rec loopb w z x y = 
	if (Random.bool ()) then
		let x = if (w mod 2 = 1) then x+1 else x in
		let y = if (z mod 2 = 0) then y+1 else y in
		loopb w z x y
	else x, y		

let rec loopa w z x y =
	if (Random.bool ()) then
		let x, y = loopb w z x y in
		let z, w = loopc w z x y in
		loopa w z x y
	else assert (x = y) 

let main () =
	let w = 1 in
	let z = 0 in
	let x = 0 in
	let y = 0 in
	loopa w z x y	
	

let _ = main ()
let _ = main ()

