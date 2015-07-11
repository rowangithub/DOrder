let rec loop x n =
	if (Random.bool ()) then
		let x = x + 1 in
		if (x >= n) then assert (x = n)
		else loop x n
	else loop x n

let main n = 
	let x = 0 in
  	if (n > 0) then 
  		loop x n
  	else ()
  
let _ = main 2
let _ = main (-2)