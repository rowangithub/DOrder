let rec loop sn x = 
	if (Random.bool ()) then
		let sn = sn + 1 in
		let x= x + 1 in
		let _ = assert (sn = x || sn = 0) in
		loop sn x 
	else ()

let main () =
	let sn = 0 in
	let x = 0 in
	loop sn x

let _ = main ()