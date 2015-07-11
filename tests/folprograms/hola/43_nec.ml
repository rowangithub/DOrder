(*
 * Based on ex16 from NECLA Static Analysis Benchmarks
 *)

let rec loop x y t = 
	if (Random.bool ()) then
		let y = if (x > 0) then y + x else y in
		loop x y t
	else assert (y >= t)

let main x y =
	let i = 0 in
	let t = y in
	if (x = y) then ()
	else 
		loop x y t
		
let _ = main 1 1
let _ = main (-1) (-5)
let _ = main 3 4		
		