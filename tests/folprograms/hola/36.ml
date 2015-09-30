(** To prove must increase the power of atomic predicates *)

let rec loopc i j k = 
	if (i<j) then loopc (i+1) j (k+1)
  else i, k
	
let rec loopd x y (z:int) = 
	if (Random.bool ()) then 
  	let x, y = 
			if(x mod 2==0) then x+2, y-2 else x-1, y-1 in
    loopd x y z
	else x, y	
	
let rec loopa w z = 
	if (Random.bool ()) then
    let i = z in
    let j = w in
    let k = 0 in
    let i, k = loopc i j k in

    let x = z in
    let y = k in

    let x, y = 
			if(x mod 2 = 1) then x+1, y-1 else x, y in

    let x, y = loopd x y z in
    let z = z+1 in
    let w = x+y+1 in
		loopa w z
	else assert (w>=z)
	
let rec loopb flag a b c d = 
  if (Random.bool ()) then
  	let c = c+1 in
    let d = d+1 in
    let a,b =
			if(flag > 0) then a+1, b+1
			else a+c, b+d in
		loopb flag a b c d 
  else assert (a=b) 	
	
let main flag =
  let z = 0 in
  let w = 0 in
	let _ = loopa w z in
	let a = 0 in
  let b = 0 in
	let c = 0 in
  let d = 0 in
	loopb flag a b c d
	

let _ = main (-1)	