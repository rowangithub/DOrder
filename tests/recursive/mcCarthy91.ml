(*
 * Implementation the McCarthy 91 function.
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 * 
 * Author: Matthias Heizmann
 * Date: 2013-07-13
 * 
 *)


let rec f91 x =
    if (x > 100) then x - 10
    else 
			f91 (f91 (x+11))
 


let main () =
    let x = Random.int 1024 in
    let result = f91 x in
    if (result = 91 || x > 101 && result = x - 10) then 0
    else 
			assert false

let _ = main ()