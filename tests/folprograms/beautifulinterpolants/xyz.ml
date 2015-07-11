let rec loopa x y z = 
	if (Random.bool()) then 
		loopa (x+1) (y+1) (z-2)
	else x, y, z

let rec loopb x y z = 
	if (x > 0) then 
		let z = z + 1 in
		let z = z + 1 in
		let x = x - 1 in
		let y = y - 1 in
		loopb x y z
	else assert (z > (-1))

let main() =
	let x = 0 in
	let y = 0 in
	let z = 0 in
 
	let x, y, z = loopa x y z in
	loopb x y z

let _ = main ()