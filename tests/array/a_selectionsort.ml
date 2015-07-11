let rec loop (i:int) (j:int) (n:int) (a: int array) =
	if (j < n) then
		if (a.(i) > a.(j)) then
			let tmp = a.(i) in
			let _ = a.(i) <- a.(j) in
			let _ = a.(j) <- tmp in
			loop i (j+1) n a
		else loop i (j+1) n a
	else ()
	
let rec sort (i:int) (n:int) (a:int array) = 
	if (i < (n - 1)) then 
		let _ = loop i (i+1) n a in
		sort (i+1) n a
	else ()
	
let main vec ith = 
	let n = Array.length vec in
	sort 0 n vec
	
let vec1 = [|90; -5; -30; -10;|]
let _ = main vec1 2
		
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -50|]
let _ = main vec1 2
let vec2 = [|20; 80; 70; 10; 30; 50; 60; 40|]
let _ = main vec2 2
let vec3 = [|100; 80; 70; 10; 30; 50; 15; 40|]
let _ = main vec3 2*)