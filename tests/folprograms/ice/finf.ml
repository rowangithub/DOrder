let rec loop i x y n = 
	if (Random.bool ()) then
		let _ = assert (x = 0) in
		let i = i + 1 in
		loop i x y n
	else assert (x = 0)	

let main n =
	let i = 0 in
 	let x = 0 in
  let y = 0 in
  if (n > 0) then
  	let i = 0 in
		loop i x y n
	else ()
	

let _ = main 1
let _ = main (-1)