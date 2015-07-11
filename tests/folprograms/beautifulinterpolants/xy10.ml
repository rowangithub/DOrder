let rec loopa x y (z:int) = 
	if (Random.bool ()) then
		loopa (x+10) (y+1) z
	else 
		assert (x > z || y < z+1)


let main z =
  let x = 0 in
  let y = 0 in
 
	loopa x y z
	
let _ = main 5
let _ = main (-4)
