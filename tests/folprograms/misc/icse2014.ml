let rec loop x y =
	if x <= 10 then
		if (x >= 5) then loop (x+1) (y+1)
		else loop (x+1) y
	else assert (y = 11)

let main x = 
	let y = 5 in
	let x = if (x > y) then y else x in
	loop x y
	
let _ = main 5
let _ = main 11
let _ = main (-2)	