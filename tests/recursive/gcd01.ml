(*
 * Recursive implementation of the greatest common denominator
 * using Euclid's algorithm
 * 
 * Author: Jan Leike
 * Date: 2013-07-17
 * 
 *)


(* Compute the greatest common denominator using Euclid's algorithm *)
let rec gcd y1 y2 =
    if (y1 <= 0 || y2 <= 0) then 0
    else if (y1 = y2) then y1
    else if (y1 > y2) then
        gcd (y1 - y2) y2
    else gcd y1 (y2 - y1)

let main () =
    let m = Random.int 32 in
    if (m <= 0 || m > 2147483647) then 0
   	else
    	let n = Random.int 32 in
    	if (n <= 0 || n > 2147483647) then 0
			else
      	let z = gcd m n in
    		if (z < 1 && m > 0 && n > 0) then
					assert false
				else z

let _ = main ()   