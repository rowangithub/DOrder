let rec loop i len y = 
	if (i < y) then
		let _ = assert (0 <= i && i < len) in
		let i = i + 1 in
		loop i len y
	else ()

let main x y =
	if (x < 0 || y < 0 || y > x) then ()
  else
		let len = x in
  	let i = 0 in
  	loop i len y