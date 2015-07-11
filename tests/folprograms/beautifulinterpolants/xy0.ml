let rec loopa x y = 
	if (Random.bool ()) then
		loopa (x+1) (y+1)
	else x, y
	
let rec loopb x y =
	if (x > 0) then 
		loopb (x-1) (y-1)
	else assert (y > (-1))	

let main () =
	let x = 0 in
 	let y = 0 in

  let x, y = loopa x y in
	loopb x y
	
let _ = main ()