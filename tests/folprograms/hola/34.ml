let rec loop i x y m n = 
	if (i < n) then 
		let i = i + 1 in
		let x = x + 1 in
		let y = if (i mod 2 = 0) then y + 1 else y in
		loop i x y m n
	else if (i = m) then assert (x = 2 * y)
	else ()

let main n = 
	let x = 0 in
	let y = 0 in
	let i = 0 in
	let m = 10 in
	loop i x y m n
	
let _ = main 10
let _ = main 11
let _ = main 9
let _ = main (-1)