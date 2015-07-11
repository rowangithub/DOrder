let rec init i n (a:int array) =
	if (i >= n) then ()
	else 
		let _ = Array.set a i (Array.get a (i-1)) in
		init (i+1) n a
		
let main vec ith = 
	let n = Array.length vec in
	let _ = init 1 n vec in
	if (0 < ith && ith < n) then
		assert (Array.get vec (ith-1) <= Array.get vec ith)
	else ()

let vec1 = [|0; -300; 100; -500; 400|]
let vec2 = [|0; 2000; -3000; 4000; -5000|]
let vec3 = [|0; 900; -800; 100; 200|]
let _ = main vec1 2
let _ = main vec2 2
let _ = main vec3 2