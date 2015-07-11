let rec loop (i:int) (j:int) (n:int) (a: int array) =
	if (j < n) then
		if (a.(i) > a.(j)) then
			let tmp = a.(i) in
			let _ = a.(i) <- a.(j) in
			let _ = a.(j) <- tmp in
			loop i (j+1) n a
		else loop i (j+1) n a
	else ()
	
let main i vec = 
	let n = Array.length vec in
	loop i (i+1) n vec
	
let vec1 = [|200; -300; -100; 900; -50|]
let _ = main 0 vec1
let vec2 = [|20; 80; 70; 10; 30;|]
let _ = main 1 vec2
let vec3 = [|100; 80; 30; 50; 40|]
let _ = main 3 vec3
	
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -50|]
let _ = main 0 vec1
let vec2 = [|20; 80; 70; 10; 30; 50; 60; 40|]
let _ = main 1 vec2
let vec2 = [|100; 80; 70; 10; 30; 50; 15; 40|]
let _ = main 3 vec2*)