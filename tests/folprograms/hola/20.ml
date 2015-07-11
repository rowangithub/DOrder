let rec loop j i k m n x y = 
	if (j < n) then
		let x = if (j = i) then x + 1 else x - 1 in
		let y = if (j = i) then y - 1 else y + 1 in	
		let b = Random.bool () in
		let m = if (b) then j else m in
		let j = if (b) then j+1 else j in
		loop (j+1) i k m n x y
	else (
		let _ = assert (x + y = k) in
		if (n > 0) then 
			assert (0 <= m && m < n)
		else ()
	)
			

let main x y k i n = 
	if (x + y = k) then
		let m = 0 in
		let j = 0 in
		loop j i k m n x y
	else ()
			
let _ = main 3 5 8 4 8 
let _ = main 3 5 8 4 (-1)
let _ = main (-1) (-1) (-1) (-1) (-1)