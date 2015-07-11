let rec loopb (a:int) b c (d:int) = 
	if (Random.bool ()) then loopb a (b-1) (c-1) d
	else a, b, c, d
	
let rec loopa a b c d =
	if (Random.bool ()) then
		let x = a + c in
		let y = b + d in
		let a, d = 
			if (x + y) mod 2 = 0 then a+1, d+1
			else a-1, d in
		(*let a, b, c, d = loopb a b c d in*)
		loopa a b c d 
	else assert (a+c = b+d)

let main () = 
	let a = 1 in
  let b = 1 in
  let c = 2 in
  let d = 2 in
	loopa a b c d 
		

let _ = main ()
			
			
 