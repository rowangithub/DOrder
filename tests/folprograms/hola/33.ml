(** The problem is that it took so long time to verify *)

let rec loopc x y = 
	if (Random.bool ()) then
		loopc (x-1) (y-1)
	else x, y	

let rec loopb z k x y c = 
	if (Random.bool ()) then
		if (z = k+y-c) then
			loopb z k (x+1) (y+1) (c+1)
		else loopb z k (x+1) (y-1) (c+1)
	else x, y	

let rec loopa z k x y = 
	if (Random.bool ()) then
		let c = 0 in
		let x, y = loopb z k x y c in
		let x, y = loopc x y in
		let z = k + y in
		loopa z k x y
	else assert (x = y)

let main k =
  let z = k in
  let x = 0 in
  let y = 0 in
	loopa z k x y
				
	
let _ = main 5
let _ = main 0
let _ = main (-5)