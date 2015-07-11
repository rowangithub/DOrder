let rec loop x y = 
	if (y >= 0) then 
		let x = x + 1 in
		let y = 
			if x <= 50 then y+1
			else y-1 in
		loop x y
	else 
		assert (x = 102)

let main () = 
	let x = 0 in
	let y = 0 in
	let y = 
		if (x <= 50) then y + 1 else y - 1 in
	loop x y
	
let _ = main ()		