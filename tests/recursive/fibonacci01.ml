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
    if (x > 46 || x == -2147483648) then 0
    else
    	let result = fibonacci x in
    	if (result >= x - 1) then 0
    	else 
				assert false
				
let _ = main 2				
let _ = main 3
let _ = main 10
let _ = main (-5)