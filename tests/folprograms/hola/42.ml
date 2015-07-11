(* Figure out why verification is quite slow *)

let rec loop flag a x y = 
	if (Random.bool ()) then
		let a, x, y = if flag > 0 then x+y, x+1, y else x+y+1, x, y+1 in
		let x, y = if a mod 2 = 1 then x, y+1 else x+1, y in
		loop flag a x y
	else
		let a = if (flag > 0) then a+1 else a in
		assert (a mod 2 = 1)
	
let main flag =
	let x = 1 in
  let y = 1 in
  let a = if flag > 0 then 0 else 1 in
	loop flag a x y
  
let _ = main 5
let _ = main (-5)