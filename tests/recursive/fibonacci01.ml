(*
 * Recursive computation of fibonacci numbers.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 *)

let rec fibonacci n =
		if (n = 1) then 1
    else if (n < 1) then 0
    else fibonacci(n-1) + fibonacci(n-2)
   
let main x =
    if (x > 46 || x == -2147483648) then ()
    else
    	let result = fibonacci x in
			assert (result >= x - 1)
    	

let _ = main 10
let _ = main (-5)