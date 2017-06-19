let f x = 
	let r = 
		if (x > 5) then 2
		else 5 in
	assert (r = 2)
	
let main x =
	if (x > 10) then
		f x
	else ()		  	