let rec loop x y =
	if x < 11 then
		if (x >= 5) then loop (x+1) (y+1)
		else loop (x+1) y
	else y

let main x = 
	let y = 5 in
	let x = if (x > y) then y else x in
	let res = loop x y in
	assert (res = 11)
	

let _ = main 12
let _ = main (-2)	