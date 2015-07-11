let rec merge i j (p:int) m q k (a: int array) (result: int array) =
	if (i < m && j < q) then
		(if (a.(i) <= a.(j)) then
			let _ = result.(k) <- a.(i) in
			merge (i+1) j p m q (k+1) a result
		else
			let _ = result.(k) <- a.(j) in
			merge i (j+1) p m q (k+1) a result)	 
	else if (i < m) then
		let _ = result.(k) <- a.(i) in
		merge (i+1) j p m q (k+1) a result
	else if (j < q) then
		let _ = result.(k) <- a.(j) in
		merge i (j+1) p m q (k+1) a result 
	else ()

	
let rec copy (p:int) i k n (result: int array) (a: int array) =
	if (k < n) then
		let _ = a.(i) <- result.(k) in
		copy p (i+1) (k+1) n result a
	else () 


let rec mergesort p q (a: int array) =
	if (p < (q-1)) then
		let m = (p + q) / 2 in
		let _ = mergesort p m a in
		let _ = mergesort m q a in
		let result = Array.make (q-p) 0 in 
		let _ = merge p m p m q 0 a result in
		(*let _ = a.(p) <- a.(p) in*)
		copy p (p) 0 (q-p) result a
	else ()
	
let main (vec: int array) ith = 
	(let n = Array.length vec in
	let _ = mergesort 0 n vec in 
	if (0 <= ith && ith < (n-1)) then
		assert (Array.get vec (ith) <= Array.get vec (ith+1))
	else ())	

let vec1 = [|90; -5; 2; 20; -30; -10;|]
let _ = main vec1 2	
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -800; 150; 300|]*)
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -800|]
let vec2 = [|20; -40; 30; -50; 10; -60; 80; -100|]*)
(*let vec2 = [|-1000; 2000; -3000; 4000; -5000|]
let vec3 = [|-100; 900; -800; 100; 200|]*)
(*let _ = main vec1 2
let _ = main vec2 2*)
(*let _ = main vec2 2
let _ = main vec3 2	*)