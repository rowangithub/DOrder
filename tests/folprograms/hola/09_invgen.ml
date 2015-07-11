(*
 * "fragtest_simple" from InvGen benchmark suite
 *)

let rec loopa i pvlen = 
	if (Random.bool ()) then
		let i = i + 1 in
		if (i > pvlen) then
			loopa i i
		else loopa i pvlen
	else ()
	
let rec loopb t i k = 	
	if (Random.bool ()) then
		loopb i (i+1) (k+1)
	else (t, i, k)
	
let rec loopc () = 
	if (Random.bool ()) then loopc ()
	else ()
	
let rec loopd k i j n = 
	let _ = assert (k >= 0) in
	let k = k - 1 in
	let i = i - 1 in
	let j = j + 1 in
	if (j < n) then loopd k i j n
	else ()
	
let main () = 
	let k = 0 in
	let i = 0 in
	let pvlen = 0 in
	let t = 0 in
	let _ = loopa i pvlen in
	let (t, i, k) = loopb t i k in
	let _ = loopc () in
	let j = 0 in
	let n = i in
	loopd k i j n

let _ = main ()
let _ = main ()
let _ = main ()
let _ = main ()