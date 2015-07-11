let rec find (i:int) (n:int) (e:int) (a:int array) = 
	if (i >= n) then (0-1)
	else if (a.(i) == e) then i
	else if (a.(i) <> e) then find (i+1) n e a
	else (0-1)
	
let main a = 
	let n = Array.length a in
	find 0 n 350 a
	
let vec1 = [|-100; -200; -300; 350; 370|]
let _ = main vec1