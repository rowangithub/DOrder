let rec loop flag x y =
	if (flag < 1) then
		let y = if x <= 50 then y+1 else y-1 in
		let flag = if (y < 0) then 1 else flag in
		let x = if (flag < 1) then x+1 else x in
		loop flag x y
	else (
		assert (x <= 102);
		assert (y<=51);
		)

let main () = 
	let x = 0 in
	let y = 0 in
	let flag = 0 in
	loop flag x y
	
let _ = main ()