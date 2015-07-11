let rec init i n a =
	if (i < n) then
		let _ = a.(i) <- (a.(i-1) + 1) in
		init (i+1) n a
	else ()
	
let main vec = 
	let n = Array.length vec in
	init 1 n vec
	
let vec = [|0;0;0;0;0|] 	
let _ = main vec
		