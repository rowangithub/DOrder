(*
 * From "A Practical and Complete Approach to Predicate Refinement" by McMillan TACAS'06
 *)

let rec loop x y (i:int) (j:int) = 
	if (x <> 0) then
		loop (x-1) (y-1) i j
	else if (i = j) then assert (y = 0)
	else ()

let main i j = 
	let x = i in
	let y = j in
	loop x y i j
	
let _ = main 10 10
let _ = main 9 9
let _ = main 3 (-3)
let _ = main 0 3