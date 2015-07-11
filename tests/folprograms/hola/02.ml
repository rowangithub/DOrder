let rec loop z x y w = 
	if (Random.bool ()) then
		let z = z + x + y + w in
		let y = y + 1 in
		let x = if (z mod 2 = 1) then x + 1 else x in
		let w = w + 2 in
		loop z x y w
	else assert (x = y && z mod 2 = 1)

let main () = 
	let i = 1 in
	let j = 0 in
	let z = i - j in
	let x = 0 in
	let y = 0 in
	let w = 0 in
	loop z x y w
	
let _ = main ()