let rec loopa i k n = 
	if ( i < n ) then
		loopa (i+1) (k+1) n
  else k
	
let rec loopb j k = 
	if ( j > 0 ) then
		let _ = assert(k > 0) in
		let j = j-1 in
		let k = k-1 in
		loopb j k
  else ()	

let main n = 
  let k = 0 in
  let i = 0 in
  let k = loopa i k n in
 
  let j = n in
  loopb j k

let _ = main 5
let _ = main (-5)