let rec loop i = 
	if i < 1000 then
		(
		loop (i+1))
	else (assert (i <= 10000);)

let main () = 
	loop 0
	
let _ = main ()