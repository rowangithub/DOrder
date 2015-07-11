let rec loopa i k n = 
	if ( i < n ) then
		let i = i+1 in
		let v = Random.int 10 in
		if ( v > 0 ) then
	  	let k = k + v in
			loopa i k n
		else
	  	let k = k+1 in
			loopa i k n
	else k
	
let rec loopb j k n = 
	if ( j < n ) then
		let _ = assert (k > 0) in
		let j = j+1 in
		let k = k-1 in
		loopb j k n
  else ()


let main n = 
  let i = 0 in
  let k = 0 in
  let k = loopa i k n in
 	let j = 0 in
  loopb j k n

let _ = main 8
let _ = main 1
let _ = main 0
let _ = main (-2)