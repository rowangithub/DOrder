(* To prove, must resolve the return value generation *)

let rec loopb (x:int) (y:int) i j =
	if (Random.bool ()) then
		if x = y then loopb x y (i+1) j
		else loopb x y i (j+1)
	else i, j
	
let rec loopa x y i j = 
	if (Random.bool ()) then
		let i, j = loopb x y i j in
		let x, y = if i >= j then x+1, y+1 else x, y+1 in
		loopa x y i j
	else
		assert (i >= j)	

let main () = 
	let x = 0 in
  let y = 0 in
  let i = 0 in
  let j = 0 in
	loopa x y i j
	

let _ = main ()
let _ = main ()