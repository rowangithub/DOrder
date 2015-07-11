let rec loopc k j = 
	if k < j then loopc (k+1) j
	else k

let rec loopb i j m n = 
	if (j < m) then
		if (Random.bool ()) then
			let _ = assert (j >= 0) in
			let j = j + 1 in
			let k = 0 in
			let k = loopc k j in
			loopb i j m n
		else 
			let _ = assert (n+j+5>i) in
			let j = j + 2 in
			loopb i j m n
	else ()	
	
let rec loopa i m n = 
	if (i < n) then 
		let _ = loopb i i m n in
		loopa (i+4) m n
	else () 	

let main m n =
  if (m+1 < n) then
		loopa 0 m n
	else ()	
		
						
let _ = main 1 3
let _ = main (-1) (-3)