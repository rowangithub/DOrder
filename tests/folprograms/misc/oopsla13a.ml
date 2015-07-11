let rec loop flag i j a b =
	if (Random.int 100 > 4) then
		let a = a + 1 in
		let b = b + j - i in
		let i = i + 2 in
		let j = if (i mod 2 = 0) then j+2 else j+1 in
		loop flag i j a b
	else if (flag > 0) then assert (a = b)
	else ()

let main flag = 
	let a = 0 in
	let b = 0 in
	let j = 1 in
	let i = if (flag > 0) then 0 else 1 in
	loop flag i j a b
	
let _ = main 2
let _ = main 1
let _ = main (-2)
let _ = main (-1)	