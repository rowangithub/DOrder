let rec loop y = 
	if y > 0 then
		let y = y - 1 in
		loop y 
	else if false then
		assert false
	else ()	


let main y =
	loop y