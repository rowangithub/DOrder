(*
 * Adapted from ex17.c in NECLA test suite
 *)
let rec loop flag b j = 
		if b < 100 then
			let j = if flag > 0 then j+1 else j in
			let b = b + 1 in
			loop flag b j
		else if (flag > 0) then assert (j = 100)
		else () 

let main flag =
	let j = 0 in		
	loop flag 0 j	
			
let _ = main 1 
let _ = main 101
let _ = main (-1)