(*
 * Taken from "Counterexample Driven Refinement for Abstract Interpretation" (TACAS'06) by Gulavani
 *)
let rec loop x m n = 
	if (x < n) then
		if (Random.bool ()) then loop x x n
		else loop (x+1) m n
	else if (n > 0) then assert (0 <= m && m < n)
	else ()

let main n =
	let x = 0 in
  let m = 0 in
	loop x m n
	
let _ = main 10
let _ = main (-1)