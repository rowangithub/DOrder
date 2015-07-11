let rec loop x y z k =
	if (Random.bool ()) then
		let x = if (k mod 3 = 0) then x+1 else x in
		let y = y + 1 in
		let z = z + 1 in
		loop x y z k
	else 
		(assert (x = y);
		assert (y = z))	

let main () = 
	let x = 0 in
  let y = 0 in
  let z = 0 in
  let k = 0 in
	loop x y z k
		
let _ = main ()		