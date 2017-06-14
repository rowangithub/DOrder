let rec loop x y (m:int) (n:int) = 
	if (x < m) then 
		if (x < n) then
			loop (x+1) y m n
		else loop (x+1) (y+1) m n
	else y



let main m = 
	let x = 0 in
	let y = 3 in
	if (m > y && y > 0) then
		let res = loop x y m y in
		
		assert (res = m)
	else ()
	

let _ = main 10
let _ = main 2
let _ = main (-1)