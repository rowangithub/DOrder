(*
 * "nested4.c" from InvGen benchmark suite
 *)

let rec loopc i n =
	if (i < n) then
		(assert (1 <= i); loopc (i+1) n)
	else ()
	
let rec loopb i n =
	if (i < n) then
		loopb (i+1) n
	else ()

let rec loop k n l = 
	if (k < n) then
		(loopb l n; loopc l n; loop (k+1) n l)
	else ()

let main l n = 
	if (l > 0) then loop 1 n l	
	else ()
	
let _ = main 2 5
let _ = main (-1) 5
let _ = main 3 (-1)