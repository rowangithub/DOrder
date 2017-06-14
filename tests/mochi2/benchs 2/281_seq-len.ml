let rec loopa i k n = 
	if i < n then loopa (i+1) (k+1) n
	else k 
	
let rec	loopb i k n = 
	if i < n then loopb (i+1) (k-1) n
	else k
	
let rec loopc i k n = 
	if i < n then (assert (k > 0); loopc (i+1) (k-1) n)
	else ()

let main n0 n1 n2 =
	let i = 0 in
  let k = 0 in

	let k = loopa i k n0 in
	let k = loopa i k n1 in
  let k = loopa i k n2 in
	let k = loopb i k n2 in
  let k = loopb i k n1 in 
	loopc i k n0 

let _ = main 2 3 4
let _ = main 3 2 4
let _ = main (-2) 2 3
let _ = main 2 (-2) 3
let _ = main 3 2 (-2)