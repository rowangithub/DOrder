(*
 * Based on "larg_const.c" from InvGen test suite  
 *)

let rec loop i n k c1 c2 = 
	if (i < n) then
		let i = i + 1 in
		let v = 
			if (Random.int 100) mod 2 = 0 then 0 else 1	in
		let k = if (v = 0) then k + c1 else k + c2 in
		loop i n k c1 c2
	else assert (k > n)	

let main () =
  let c1 = 4000 in
  let c2 = 2000 in
  
  let n = 1 + Random.int 9 in
  let k = 0 in
  let i = 0 in
	loop i n k c1 c2
	
let _ = main ()
let _ = main ()