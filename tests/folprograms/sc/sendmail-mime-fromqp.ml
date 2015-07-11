let rec loop outfilelen nchar out = 
	if(Random.bool ()) then
    if(Random.bool ()) then out
		else
			if(Random.bool ()) then
				let out = 0 in
				let nchar = 0 in
				loop outfilelen nchar out
      else
				if(Random.bool ()) then out
				else 
					let nchar = nchar + 1 in
					if (nchar >= outfilelen) then out
	  			else
						(assert(0<=out);
						assert(out<outfilelen);
						let out = out + 1 in
						loop outfilelen nchar out)
    else
      let nchar = nchar + 1 in
      if (nchar >= outfilelen) then out
			else
				(assert(0<=out);
      	assert(out<outfilelen);
      	let out = out + 1 in
      	if(Random.bool ()) then out
				else loop outfilelen nchar out)

 
let main outfilelen = 
	let nchar = 0 in
  let out = 0 in
  if(outfilelen > 0) then
		let out = loop outfilelen nchar out in
		(assert(0<=out);
  	assert(out<outfilelen);
		out+1)
	else 0
	
let _ = main 2
let _ = main (-2)	