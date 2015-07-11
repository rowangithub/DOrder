let rec loopb i m = 
	if ( m > 0 ) then
      loopb (2*i) (m-1)
	else i, m
	
let rec loopa i m n =	
  if ( i < n ) then
		let i, m = loopb i m in
		loopa i m n
  else assert (i > 0 )	

let main n =
	let i = 1 in
 	let m = 10 in
	loopa i m n
	


let _ = main 5
let _ = main 4
let _ = main (-2)