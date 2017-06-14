(*
 * Taken from Gulwani PLDI'08:
 * Program Analysis as Constraint Solving
 *)

let rec loop x y = 
	if (x < 0) then
		let x = x + y in
		let y = y + 1 in
		loop x y
	else assert (y > 0) 

let main y = 
	let x= -50 in
	loop x y

let _ = main 5
let _ = main (-5)