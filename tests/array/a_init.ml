let rec init (i:int) (n:int) (x:int) (a:int array) = 
	if (i >= n) then ()
	else 
		(let _ = Array.set a i x
		in init (i+1) n x a)

let main l ith = 
	let n = Array.length l in
	init 0 n 1 l
	
let vec = [|0;0;0|] 	
let _ = main vec 1