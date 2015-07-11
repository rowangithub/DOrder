(*
 * ex49 from NECLA Static Analysis Benchmarks
 *)

let rec loop i n sum = 
	if (i < n) then 
		loop (1+i) n (sum + i)
	else
		assert(sum >= 0)

let main n =
	let sum = 0 in
  if (n >= 0) then
		loop 0 n sum
	else ()
		
let _ = main 5
let _ = main (-5)