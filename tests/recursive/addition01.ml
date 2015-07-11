(*
 * Recursive implementation integer addition.
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 *)


let rec addition m n =
    if (n = 0) then m
    else if (n > 0) then
        addition (m+1) (n-1)
    else 
				addition (m-1) (n+1)
  


let main () =
    let m = Random.int 1024 in
    if (m < 0 || m > 2147483647) then 0
		else
    	let n = Random.int 1024 in
    	if (n < 0 || n > 2147483647) then 0
    	else
				let result = addition m n in
    		if (result = m + n) then 0
	    	else 
					assert false
					
let _ = main ()					