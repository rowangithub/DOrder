let rec loopa x y i j flag = 
	if (Random.bool ()) then
		let x = x + 1 in
		let y = y + 1 in
		let i = i + x in
		let j = j + y in
		let j = if (flag > 0) then j+1 else j in
		loopa x y i j flag
	else x, y, i, j
	
let rec loopc x y z w = 
	if (Random.bool ()) then
		let x = if (w mod 2 = 1) then x+1 else x in
		let y = if (z mod 2 = 0) then y+1 else y in
		loopc x y z w
	else x, y	
		
let rec loopb x y z w = 
	if (Random.bool ()) then
		let x, y = loopc x y z w in
		let z = x + y in
		let w = z + 1 in
		loopb x y z w
	else assert (x = y)			

let main flag =
  let x = 0 in
  let y = 0 in
  let j = 0 in
  let i = 0 in
  let c = 0 in
  let d = 1 in 
	let x, y, i, j = loopa x y i j flag in
  let x = 
		if(j>=i) then y else y+1 in
	let w = 1 in
  let z = 0 in
	loopb x y z w
	
let _ = main 2
let _ = main 1
let _ = main 2
let _ = main 1
let _ = main (-2)
let _ = main (-1)
let _ = main (-2)
let _ = main (-1)