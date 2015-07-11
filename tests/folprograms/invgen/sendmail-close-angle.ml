let rec loop (inf:int) (buf:int) (buflim:int) (bufferlen:int) (inlen:int) = 
	if (Random.bool ()) then
 		if (buf == buflim) then 
			(assert(0<=buf);
    	assert(buf<bufferlen);
			buf)
    else 
    	let _ = assert(0<=buf) in
    	let _ = assert(buf<bufferlen) in
    	(*buf = cur;*)
    	let buf = buf + 1 in

    	let inf = inf + 1 in
    	let _ = assert(0<=inf) in
    	let _ = assert(inf<inlen) in
 	   	(*cur = *in;*)
			loop inf buf buflim bufferlen inlen
	else 
		(assert(0<=buf);
    assert(buf<bufferlen);
		buf) 

let main bufferlen inlen = 
	if bufferlen >1 && inlen > 0 && bufferlen < inlen then 
		let buf = 0 in
  	let inf = 0 in
  	let buflim = bufferlen - 2 in
    let _ = assert(0<=inf) in
  	let _ = assert(inf<inlen) in
  	let buf = loop inf buf buflim bufferlen inlen in

  	let buf = buf + 1 in
  	let _ = assert(0<=buf) in
  	let _ = assert(buf<bufferlen) in

  	(*buf = EOS;*)
  	(buf+1; ())
	else ()

let _ = main 5 6
let _ = main 5 7	
let _ = main 5 8
let _ = main 5 9
let _ = main 6 10
let _ = main (-1) (-2)
let _ = main (-2) (-3)