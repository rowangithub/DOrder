let rec loop x y =
	if (Random.bool ()) then 
		if y >= 0 then
			loop x (y+x)
		else assert false
	else ()

let main () =
	let x = 0 in
	let y = 0 in
	loop x y

let _ = main ()