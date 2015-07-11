let rec reverse i (n:int) (a: int array) (b: int array) =
	if (i < n) then
		let _ = b.(n-i-1) <- a.(i) in
		reverse (i+1) n a b
	else ()
	
let main a = 
	let n = Array.length a in
	let b = Array.make n 0 in
	reverse 0 n a b
	
let vec1 = [|-100; -200; -300; 350; 370|]
let _ = main vec1