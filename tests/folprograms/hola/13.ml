(*
 * Based on "Property-Directed Incremental Invariant Generation" by Bradley et al.
 *)

let rec loop flag j k =
	if (Random.bool ()) then
		if (flag > 0) then
			loop flag (j+4) k
		else 
			loop flag (j+2) (k+1)
	else 
		if (k <> 0) then assert (j = 2*k+2)
		else ()

let main flag = 
	let j = 2 in
	let k = 0 in
	loop flag j k
	
let _ = main 5
let _ = main 5
let _ = main 5
let _ = main (-5)	