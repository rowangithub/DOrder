let rec loop x y = 
	if x < 100 then
		let x = x + 1 in
		let y = if (x > 50) then y+1 else y in
		loop x y
	else assert (y = 100)

let main () = 
	let x = 0 in
	let y = 50 in
	loop x y
	
let _ = main ()