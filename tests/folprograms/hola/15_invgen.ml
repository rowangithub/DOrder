(*
 * from Invgen test suite: faile because j + k >= n is the desired invariant
 *)

let rec loop j k n =
	if (j < n) then
		loop (j+1) (k-1) n
	else assert (k >= 0)

let main k n = 
	if (n > 0 && k > n) then
		let j = 0 in
		loop j k n
	else ()
	
let _ = main 4 2
let _ = main 4 3
let _ = main 2 1
let _ = main (-2) (-1)