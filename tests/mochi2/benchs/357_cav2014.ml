let rec loop i (j:int) k p n = 
	if i < n then
		if (i >= p ) then loop (i+1) k k p n
		else loop (i+1) j k p n
	else j

let main p n = 
	let i = 0 in
	let j = 0 in
	let k = 3 in
	
		let res = loop i j k p n in
	if (p > 0 && n > p) then	
		assert (res = k)
		
	else ()