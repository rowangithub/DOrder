let rec f g x =
	if x >= 0 then g x
	else 
		let p = f g in
		let q = g x in
		let s = f p q in
		s
		
let succ x = x + 1

let main n = 
	let r = f succ n in
	assert (r >= 0)

let _ = main 1
let _ = main (-1)

