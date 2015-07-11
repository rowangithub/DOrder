let rec loopb i j l n right = 
	if j <= right then
		let j = 
			if (j < right) then (
				assert(1 <= j);assert(j <= n);
				assert(1 <= j+1);assert(j+1 <= n);
				if(Random.bool ()) then j + 1 else j
  		)
			else j in
		let _ = assert(1 <= j);assert(j <= n) in
  	if(Random.bool ()) then () 
  	else
  		let _ = assert(1 <= i) in
  		let _ = assert(i <= n) in
  		let _ = assert(1 <= j) in
  		let _ = assert(j <= n) in
  		let i = j in
  		let j = 2*j in
			loopb i j l n right
	else ()

let rec loopa l n right = 
	if (right > 1) then
		let i = l in
    let j = 2*l in
		let _ = loopb i j l n right in	
		let l = 
			if (l > 1) then 
				(assert(1 <= l);assert(l <= n);
      	l-1)
			else l in
		let right = 
			if (l > 1) then right
			else
				(assert(1 <= right);assert(right <= n);
      	right-1) in
  	loopa l n right
	else ()

let main n = 
  if (1 <= n) then
  	let l = n + 1 in
  	let n = n + n in
  	let right = n in
  	let l = if (l>1) then l - 1 else l in
		let right = if (l>1) then right else right-1 in
		loopa l n right
	else ()
	
let _ = main 2
let _ = main (-1)