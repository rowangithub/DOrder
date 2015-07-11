(*
 * recHanoi.c
 *
 *  Created on: 17.07.2013
 *      Author: Stefan Wissert
 *)


(*
 * This function returns the optimal amount of steps,
 * needed to solve the problem for n-disks
 *)
let rec hanoi n =
    let result = 
			if (n = 1) then 1
			else 2 * (hanoi (n-1)) + 1 in
		(result)


let main () =
    let n = Random.int 32 in
    if (n < 1 || n > 31) then 0
   	else
    	let result = hanoi n in
			(assert (result >= n); result)
			
let _ = main ()			