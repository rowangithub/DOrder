let rec loopa cp urilen = 
	if ( cp != urilen-1) then
	  if(Random.bool ()) then cp
		else
	  	let _ = assert(cp < urilen) in
	    let _ = assert(0 <= cp) in
	    loopa (cp+1) urilen
	else 
		let _ = assert(cp < urilen) in
    let _ = assert( 0 <= cp ) in
		cp
			
let rec loopb c cp urilen tokenlen = 
	if ( cp != urilen-1
             && c < tokenlen - 1) then
		let _ = assert(cp < urilen) in
		let _ =	assert(0<=cp) in
    if (Random.bool ()) then
    	let c = c + 1 in
      let _ = assert(c < tokenlen) in
	  	let _ = assert(0<=c) in
      let _ = assert(cp < urilen) in
	  	let _ = assert(0<=cp) in
      loopb c (cp+1) urilen tokenlen
    else loopb c (cp+1) urilen tokenlen
	else ()

let main urilen tokenlen scheme =
  if(urilen > 0 && tokenlen > 0 && scheme > 0 && urilen-1 >= scheme) then 
			let cp = scheme in
  
  		let _ = assert (cp-1 < urilen) in
  		let _ = assert (0 <= cp-1) in

  
   	 	let _ = assert(cp < urilen) in
    	let _ = assert(0 <= cp) in
			
	    let cp = loopa cp urilen in
    	
    	if (cp == urilen-1) then ()
			else
    		let _ = assert(cp+1 < urilen) in
    		let _ = assert( 0 <= cp+1 ) in
    		if (cp+1 == urilen-1) then () 
				else
    			let cp = cp + 1 in
					let scheme = cp in
					let c = 0 in
      		(*//token[0] = uri;*)
      		let _ = assert(cp < urilen) in
      		let _ = assert(0<=cp) in
      		loopb c cp urilen tokenlen
	else ()
	
let _ = main 10 11 9
let _ = main 10 11 5
let _ = main 21 18 13
let _ = main (-8) (-5) (-3)	  