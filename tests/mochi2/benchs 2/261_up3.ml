let rec loopa i k n = 
	if ( 2*i < 2*n ) then
		loopa (i + 2) (k + 1) n
  else k
	
let rec loopb j k n = 
	if ( 2*j < 2*n ) then
  	let _ = assert (k > 0) in
		loopb (j + 2) (k-1) n
	else ()
  

let main n =
	let i = 0 in
  let k = 0 in
  if (n >= 0) then
	let k = loopa i k n in
	
  let j = 0 in
  loopb j k n
	else ()
	
let _ = main 10
let _ = main 15
let _ = main 7
let _ = main (-5)	