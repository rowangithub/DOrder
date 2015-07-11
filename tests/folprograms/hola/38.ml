(** To prove this algorithm, must train the learning algorithm to forget predicate i < n *)

let rec loop i n x y =
	if (i < n) then 
		let i = i + 1 in
		let x = x + 1 in
		let y = if ((i+1) mod 2 = 0) then y else y+1 in
		loop i n x y
	else if (i mod 2 = 0) then assert (x = 2 * y)
	else ()

let main n = 
	let (x, y, i) = (0, 0, 0) in
	loop i n x y
	
let _ = main 100
let _ = main 99