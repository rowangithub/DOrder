(*
 * Implementation the Ackermann function.
 * http://en.wikipedia.org/wiki/Ackermann_function
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 *)

let rec ackermann m n =
    if (m=0) then n + 1
    else if (n=0) then ackermann (m-1) 1
 		else ackermann (m-1) (ackermann m (n-1))


let main m n = 
    if (m < 0 || m > 3) then ()
    else
    	if (n < 0 || n > 23) then ()
     	else 
				let result = ackermann m n in
				assert (m < 0 || n < 0 || result >= 0)
				
let _ = main 2 3
let _ = main 3 2
let _ = main (-2) (-2)				