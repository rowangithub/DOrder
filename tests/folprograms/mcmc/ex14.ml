let rec loop x = 
	if (x <= 10) then
		let y = 10 - x in
		let _ = assert (y >= 0 && y < 10) in
		let x = x + 1 in
		loop x
	else ()

let main () = 
	let x = 1 in
	loop x
	
let _ = main ()	