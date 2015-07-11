let rec loop lock x y = 	
	if (x <> y) then
		let lock = 1 in
		let x = y in
		if (Random.bool ()) then
			let lock = 0 in
			let y = y + 1 in
			loop lock x y
		else loop lock x y
	else assert (lock = 1	)

let main y = 
	let lock = 1 in
  let x = y in
  	
  let lock, y = 
  	if (Random.bool ()) then 0, y+1
  	else lock, y in
 	loop lock x y
 	
 	
let _ = main 1
let _ = main (-1)