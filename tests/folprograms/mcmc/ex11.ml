let rec loop len = 
	if (Random.bool ()) then
	let len = 
		if len = 4 then 0 else len in
	let _ = assert (0 <= len && len < 5) in
	loop (len+1)
	else ()

let main () = 
	let len = 0 in
	loop len
	
let _ = main ()