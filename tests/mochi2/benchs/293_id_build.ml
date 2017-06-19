let rec loopb i j nlen = 
	if j < 8 then 
		(assert(0 <= nlen-1-i);
    assert(nlen-1-i < nlen);
		loopb i (j+1) nlen)
	else ()

let rec loopa i nlen = 
	if i < nlen then
		(loopb i 0 nlen;
		loopa (i+1) nlen)
	else ()				

let main nlen =
  loopa 0 nlen