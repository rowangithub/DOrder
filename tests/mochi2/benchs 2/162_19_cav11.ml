(*
 * From "Simplifying Loop Invariant Generation using Splitter Predicates", Sharma et al. CAV'11
 *)

let rec loop x y m n = 
	if (x < n) then
		let x = x + 1 in
		let y = if (x > m) then y+1 else y in
		loop x y m n
	else assert (y = n)

let main n m =
	if (n >= 0 && m >= 0 && n > m) then
		let x = 0 in
		let y = m in
		loop x y m n
	else ()
	
let _ = main 5 2
let _ = main 6 4
let _ = main (-2) (-1)