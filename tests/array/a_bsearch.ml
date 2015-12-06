let rec find e l u (n:int) (a:int array) =
	if (l <= u) then
		let m = (l+u)/2 in
		if (a.(m) < e) then
			find e (m+1) u n a
		else if (a.(m) > e) then
			find e l (m-1) n a
		else m
	else (-1)
	
let main e ith n (vec:int array) = 
	let r = find e 0 (n-1) n vec in ()
	(*if (0 < ith && ith < (n-1)) then
		assert (Array.get vec (ith) <= Array.get vec (1+ith))
	else ()*)
	
let vec1 = [|-800; -500; -100; 100; 200; 400|]
let vec2 = [|-100; -50; -40; 10; 20; 80|]
let _ = main 100 2 (Array.length vec1) vec1
let _ = main (-100) 2 (Array.length vec2) vec2