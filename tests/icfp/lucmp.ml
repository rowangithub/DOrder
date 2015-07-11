let rec loop i n = 
	if i <= n then
		(assert (i < 50); loop (i+1) n)
	else ()

let main () =
	let n = 5 in
	loop 0 n
	
let _ = main ()