(*
 * InvGen, CAV'09 paper, fig 2
 *)
let rec loop x n = 
	if (x < n) then loop (x+1) n
	else if (n > 0) then assert (x = n)
	else ()

let main n =
  let x = 0 in
	loop x n
	
let _ = main 2
let _ = main 0
let _ = main (-1)