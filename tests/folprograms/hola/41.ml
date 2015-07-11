(*
 * Adapted from "Automated Error Diagnosis Using Abductive Inference" by Dillig et al.
 *)

(** To prove, must use more inequality predicate *)
let rec loop i j k n = 
	if (i <= n) then 
		let i = i + 1 in
		let j = j + i in
		loop i j k n
	else 
		let z = k + i + j in
		assert (z > 2 * n) 

let main n flag =
	if (n >= 0) then
		let k = 1 in
		let k = 
			if (flag > 0) then Random.int 100 else k in
		let i = 0 in
		let j = 0 in
		loop i j k n
	else ()
		
let _ = main 10 5
let _ = main 10 (-5)
let _ = main (-10) 5
 