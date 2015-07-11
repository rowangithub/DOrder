(*
 * From "The Octagon Abstract Domain" HOSC 2006 by Mine.
 *)

let rec loop a j m = 
	if (j <= m) then
		if (Random.bool ()) then loop (a+1) (j+1) m
		else loop (a-1) (j+1) m
	else (assert (a >= (0-m) && a <= m))

let main m = 
	let a = 0 in
	if (m <= 0) then ()
	else
		loop a 1 m
		
let _ = main 5
let _ = main (-5)