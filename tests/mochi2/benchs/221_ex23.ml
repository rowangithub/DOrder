let rec loop (y:int) z c = 
	if (c < 36) then
		(assert (0 <= z && z < 4608);
		loop y (z+1) (c+1))
	else ()	

let main y = 
	let c = 0 in
	if (y >= 0 && y <= 127) then 
		let z = y * 36 in
		loop y z c
	else ()

let _ = main (-1)		
let _ = main 0	
let _ = main 127
let _ = main 128