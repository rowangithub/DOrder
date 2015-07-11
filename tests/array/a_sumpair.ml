let rec sum e l u (n:int) (a:int array) =
	if (l < u) then
		if (a.(l) + a.(u) < e) then
			sum e (l+1) u n a
		else if (a.(l) + a.(u) > e) then
			sum e l (u-1) n a
		else ()
	else ()	
	
let main e ith n (vec:int array) = 
	(sum e 0 (n-1) n vec;
	if (0 <= ith && ith < (n-1)) then
		assert (vec.(ith) <= vec.(1+ith))
	else ())
	
let vec1 = [|-800; -500; -300; 400; 900|]
let vec2 = [|-100; -60; 30; 80|]
let _ = main 50 2 (Array.length vec1) vec1
let _ = main 50 2 (Array.length vec2) vec2