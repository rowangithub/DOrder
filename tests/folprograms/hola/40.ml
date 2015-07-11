(** Must be able to remove predicates from counterexample;
		Must be able to infer atomic predicates that need extra constants *)

let rec loopa (flag:int) i j = 
	if (Random.bool ()) then
		let i = i + 2 in
		let j = if (i mod 2 = 0) then j+2 else j+1 in
		loopa flag i j
	else 
		( (i, j))
	
let rec loopb flag a b j i = 
	if (Random.bool ()) then 
		loopb flag (a + 1) (b + j - i) j i
	else if (flag > 0) then
		assert (a = b)
	else ()
	
let main flag = 
	let j = 1 in
	let i = if (flag > 0) then 0 else 1 in
	let (i, j) = loopa flag i j in
	let a = 0 in
	let b = 0 in
	loopb flag a b j i
	

let _ = main 30
let _ = main 20
let _ = main 10
let _ = main 0
let _ = main (-10)
let _ = main (-20)
let _ = main (-30)