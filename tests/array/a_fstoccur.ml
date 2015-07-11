let rec find e l u (n:int) (a:int array) =
	if (l < u) then
		let m = (l+u)/2 in
		if (a.(m) < e) then
			find e (m+1) u n a
		else if (a.(m) >= e) then
			find e l m n a
		else ()
	else ()
	
let main e ith n (vec:int array) = 
	(find e 0 n n vec;
	if (0 <= ith && ith < (n-1)) then
		assert (Array.get vec (ith) <= Array.get vec (1+ith))
	else ())
	
let vec1 = [|-800; -100; 100; 200; 400|]
let vec2 = [|-100; -40; 10; 20; 30|]
let _ = main 100 2 (Array.length vec1) vec1
let _ = main 20 2 (Array.length vec2) vec2
	
  