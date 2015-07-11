(*
 * Based on ex3 from NECLA Static Analysis Benchmarks
 *)

let rec loop i j x = 
	if (i < x) then loop (i+1) (j+2) x
	else assert (j = 2 * x)

let main () = 
	let j = 0 in
	let x = 100 in
	loop 0 j x
	
let _ = main ()