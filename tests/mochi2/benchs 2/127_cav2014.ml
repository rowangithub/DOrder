let rec loop i j p = 
	if i < 100 then
		if (i > p) then loop (i+1) 1 p
		else loop (i+1) j p
	else j

let main p = 
	let i = 0 in
	let j = 0 in
	if (p >= 25 && p < 75) then	
		let res = loop i j p in
		assert (res = 1)
	else ()


let _ = main (102)
let _ = main (-1)