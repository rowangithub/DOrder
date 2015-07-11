let rec loop i x = 
	if (Random.bool ()) then
		(assert (x = 0);
		loop (i+1) x)
	else 
		assert (x = 0)
		
let main n =
	let x = 0 in 
	if n > 0 then
		let i = 0 in
		loop i x
	else ()
	
let _ = main 2
let _ = main (-2)	