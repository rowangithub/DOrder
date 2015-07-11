let rec loop x y i j flag = 
	if (Random.bool ()) then
		let x = x + 1 in
		let y = y + 1 in
		let i = i + x in
		let j = j + y in
		let j = if (flag > 0) then j + 1 else j in
		loop x y i j flag
	else assert (j >= i)
		

let main flag = 
	let x = 0 in
	let y = 0 in
	let j = 0 in
	let i = 0 in
	loop x y i j flag
	
let _ = main 5
let _ = main (-5)