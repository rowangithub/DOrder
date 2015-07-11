let rec copy (i:int) (n:int) (a:int array) = 
	if (i >= n) then ()
	else 
		(let _ = a.(i) <- a.(i) in
		copy (i+1) n a)

let main (l:int array) = 
	let n = Array.length l in
	copy 0 n l
	
	
let vec = [|100;-100;-200;300;50|] 	
let _ = main vec