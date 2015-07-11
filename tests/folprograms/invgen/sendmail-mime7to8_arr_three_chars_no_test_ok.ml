let loop fb fbuflen = 
	 if (Random.bool ()) then
   	let _ = assert(0<=fb) in
    let _ = assert(fb<fbuflen) in
    let fb = fb + 1 in
    let fb = 
    	if (fb >= fbuflen-1) then 0 else fb in

    
    let _ = assert(0<=fb) in
    let _ = assert(fb<fbuflen) in

   
    let fb = fb + 1 in
    let fb = if (fb >= fbuflen-1) then 0 else fb in
      

    let _ = assert(0<=fb) in
    let _ = assert(fb<fbuflen) in

    
    let fb = fb + 1 in
    if (fb >= fbuflen-1) then 0 else fb
	else fb
  

let main fbuflen =
  if (fbuflen >0) then
  	let fb = 0 in
		let fb = loop fb fbuflen in 
		(* force out partial last line *)
  	if (fb > 0) then
    	let _ = assert(0<=fb) in
    	let _ = assert(fb<fbuflen) in ()
      (*fbuf[fb] = EOS;*)
  	else ()
	else ()
		
let _ = main 6
let _ = main 8
let _ = main (-3)