let rec loopa i k n = 
	if ( i < n ) then
		loopa (i+1) (k+1) n
  else k
	
let rec loopb j k n = 	
	if ( j < n ) then
    let _ = assert (k > 0) in
    loopb (j+1) (k-1) n
  else ()

let main n =
	let i = 0 in
  let k = 0 in
  let k = loopa i k n in
  
  let j = 0 in
	loopb j k n

let _ = main 5
let _ = main (-5)