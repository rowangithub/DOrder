(*Important: The original coefficients and constants are so large that 
 we learn from tests so slow ... We change the coefficient and constant
 to a small value ...*)

let rec loopa i k n0 = 
	if i < 2*n0 then 
		loopa (i+1) (k+1) n0
	else k
	
let rec loopb i k n1 = 
	if i < n1+4 then
		loopb (i+1) (k+1) n1
	else k	
	
let rec loopc i k n1 = 
	if i < n1+4 then
		let _ = assert (k > 0) in
		loopc (i+1) (k-1) n1
	else k
	
let rec loopd i k n0 =
	if i < 2*n0 then
		let _ = assert (k > 0) in
		loopd (i+1) (k-1) n0
	else ()	
		

let main n0 n1 =
	let i = 0 in
  let k = 0 in
	let k = loopa i k n0 in
	
  let i = 0 in
	let k = loopb i k n1 in
	
  let i = 0 in
	let k = loopc i k n1 in
	
	
  let i = 0 in
  loopd i k n0

	
let _ = main 1 2
let _ = main 2 1
let _ = main (-10) (-10)
let _ = main (-10) 10
let _ = main 10 (-10)