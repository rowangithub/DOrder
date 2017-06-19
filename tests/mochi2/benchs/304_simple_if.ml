let rec loop i m n = 
  if ( i < n ) then
    if( m > 0 ) then 
      loop (2*i) m n
    else 
      loop (3*i) m n
  else assert (i > 0 )

let main m n =
	let i = 1 in
  loop i m n