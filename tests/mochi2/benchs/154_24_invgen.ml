(*
 * "nested5.c" from InvGen test suite
 *)


let rec loopc i k n	=
	if k < n then
		(assert (k >= i); loopc i (k+1) n)
	else ()
	
let rec loopb i j n = 
	if j < n then
		(loopc i j n; loopb i (j+1) n)
	else ()	
	
let rec loopa i n = 
	if i < n then
		(loopb i i n; loopa (i+1) n)
	else ()	
	
let main n = loopa 0 n