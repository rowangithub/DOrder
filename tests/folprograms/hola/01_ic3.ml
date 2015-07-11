(*
 * IC3 motivating example
 *) 
let rec loop x y = 
	if (Random.bool ()) then
		let t1 = x in
		let t2 = y in
		loop (t1 + t2) (t1 + t2)
	else assert (y >= 1)

let main () = 
	let x = 1 in
	let y = 1 in
	loop x y
	
let _ = main ()