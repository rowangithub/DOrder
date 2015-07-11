let rec loop i j len base_sz = 
	if ( len == 0 ) then ()
  else       
		let _ = assert( 0<= j ) in
		let _ = assert(j < base_sz) in
    let _ = assert( 0<= i ) in
		let _ = assert(i < base_sz ) in
    (*      buf[j] = str[i];*)
    if ( Random.bool () ) then
        let i = i+1 in
        let j = j+1 in
        ()
		else loop (i+1) (j+1) (len-1) base_sz
    

let main base_sz = 
	let len = base_sz in

  if(base_sz > 0 ) then
		let _ = assert( 0 <= base_sz-1 ) in

  	if (len == 0) then ()
		else 
			let i = 0 in
  		let j = 0 in
  		loop i j len base_sz
	else ()

let _ = main 5
let _ = main 10
let _ = main (-1)