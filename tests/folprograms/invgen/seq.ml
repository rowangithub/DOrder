let rec loopa i k n = 
	if i < n then loopa (i+1) (k+1) n
	else k 
	
	
let rec loopc i k m n = 
	if i < m + n then (assert (k > 0); loopc (i+1) (k-1) m n)
	else ()

let main n m =
	let i = 0 in
  let k = 0 in

	let k = loopa i k n in
	let k = loopa i k m in
	loopc i k m n

let _ = main 2 3
let _ = main 3 2
let _ = main (-1) (-1)
let _ = main (-2) 2
let _ = main 2 (-2)