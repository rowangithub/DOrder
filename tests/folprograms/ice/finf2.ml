let rec loop i x = 
	if (Random.bool ()) then
		let _ = assert (x = 0) in
		loop (i+1) x
	else assert (x = 0)
	
let main () = 
	let i = 0 in
	let x = 0 in
	loop i x
	
let _ = main ()	