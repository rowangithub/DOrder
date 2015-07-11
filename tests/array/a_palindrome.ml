let rec palindrome i (n:int) (a: int array) =
	if (i < n/2) then
		if (a.(n-i-1) = a.(i)) then 
			palindrome (i+1) n a
		else ()
	else ()
	
let main vec = 
	let n = Array.length vec in
	palindrome 0 n vec
	
let vec1 = [|-100; -200; -300; 350; -300; -200; -100|]
let _ = main vec1