let rec loop n x y = 
	if (x < n) then loop n (x+1) (y+2)
	else assert (y >= n)

let main n = 
	let x = 0 in
	let y = 0 in
	loop n x y 
	
let _ = main 3
let _ = main (-3)