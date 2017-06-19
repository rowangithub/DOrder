let rec loopa i0 k n0 = 
	if i0 < n0 then 
		loopa (i0+1) (k+1) n0
	else k
	
let rec loopb i1 k n1 = 
	if i1 < n1 then
		loopb (i1+1) (k+1) n1
	else k	
	
let rec loopc j1 k n1 = 
	if j1 < n1 then
		let _ = assert (k > 0) in
		loopc (j1+1) (k-1) n1
	else k
	
let rec loopd j0 k n0 =
	if j0 < n0 then
		let _ = assert (k > 0) in
		loopd (j0+1) (k-1) n0
	else ()	
		

let main n0 n1 =
	let i0 = 0 in
  let k = 0 in
	let k = loopa i0 k n0 in
	
  let i1 = 0 in
	let k = loopb i1 k n1 in
	
  let j1 = 0 in
	let k = loopc j1 k n1 in
	
	
  let j0 = 0 in
  loopd j0 k n0