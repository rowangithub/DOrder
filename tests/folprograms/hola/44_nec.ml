(*
 * Adapted from ex20 from NECLA Static Analysis Benchmarks
 *)

(* To prove, must remove i <= k from predicate list 

qualif Loop(v) : n = 1
qualif Loop(v) : n = 2*)

let rec loop i j k n flag = 
	if (i <= k) then
		loop (i+1) (j+n) k n flag
	else if (flag > 0) then
    assert(j == i)
	else ()

let main k flag =
	let i = 0 in
  let j = 0 in
  let n = 
		if (flag > 0) then 1
		else 2 in
	loop i j k n flag
	

let _ = main (-1) (4)
let _ = main (-2) (-1)