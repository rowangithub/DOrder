let rec loop n x y = 
	if (x < n) then loop n (x+1) (y+2)
	else y

let main n = 
	let x = 0 in
	let y = 0 in
	let res = loop n x y in
	assert (res >= n)