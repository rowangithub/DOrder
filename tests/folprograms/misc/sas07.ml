let rec loop x y = 
	if (y = 0 && x > 0) then assert (x = 102)
	else
		let y = 
			if x <= 50 then y+1
			else y-1 in
		loop (x+1) y

let main () = 
	let x = 0 in
	let y = 0 in
	loop x y
	
let _ = main ()	