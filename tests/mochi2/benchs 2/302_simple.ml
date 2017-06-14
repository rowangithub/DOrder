let rec loop x n = 
	if ( x < n ) then
    loop (x+1) n
  else
  	assert( x<=n )

let main n =
  let x=0 in
  
  if (n > 0 ) then
		loop x n
	else ()

let _ = main 5
let _ = main (-4)