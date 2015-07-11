let rec loopa i k n = 
	if ( 2*i < 2*n ) then
    let k = k - 1 in
    let i = i + 2 in
		loopa i k n
  else k

let rec loopb j k n = 
	if ( j < n/2 ) then
    let _ = assert(2*k > 0) in
    let k = k-1 in
    let j = j+1 in
		loopb j k n
  else ()

let main n = 
  if (n>=0) then
  	let k = n in
  	let i = 0 in
		let k = loopa i k n in
		
		let j = 0 in
 		loopb j k n
	else ()
	
let _ = main 10
let _ = main 19
let _ = main (-4)	