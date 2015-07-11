(*
 * Based on "SYNERGY: A New Algorithm for Property Checking" by Gulavani et al.
 *)

let rec loop i c = 
	if i < 1000 then
		loop (c+i) (i+1)
	else assert (c >= 0)

let main () = 
	let i = 0 in
	let c = 0 in
	loop i c 

let _ = main ()