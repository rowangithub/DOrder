let rec loopa z k = 
	if (z < k) then loopa (2*z) k
	else (assert (z >= 1); z)

let rec loopb x y z d = 
	if (x > 0 && y > 0) then
		let c = Random.int 10 in
		let x, y, z = 
			if (c = 0) then x, y - d, z 
			else x-d, Random.int 10, z-1 in
		loopb x y z d
	else ()

let main () = 
	let x = if (Random.bool ()) then Random.int 10 else (0 - Random.int 10) in
	let y = if (Random.bool ()) then Random.int 10 else (0 - Random.int 10) in
	let k = if (Random.bool ()) then Random.int 10 else (0 - Random.int 10) in
	let d = if (Random.bool ()) then Random.int 10 else (0 - Random.int 10) in
	let z = 1 in
	let z = loopa z k in
	loopb x y z d
	
let _ = main ()