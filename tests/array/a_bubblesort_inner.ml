let rec loop (i:int) (j:int) (n:int) (a: int array) = 
	if (j < n-i-1) then
		if (a.(j) >= a.(j+1)) then
			let tmp = a.(j) in
			let _ = a.(j) <- a.(j+1) in
			let _ = a.(j+1) <- tmp in
			loop i (j+1) n a
		else loop i (j+1) n a
	else ()
	
let main i vec = 
	let n = Array.length vec in
	loop i 0 n vec
	
let vec1 = [|200; -300; -100; 900; -50|]
let _ = main 0 vec1
let vec2 = [|20; 80; 70; 10; 30;|]
let _ = main 1 vec2
let vec3 = [|100; 80; 30; 50; 40|]
let _ = main 3 vec3	
(*let vec4 = [|400; -100; 900; -50; 15|]
let _ = main 0 vec4
let vec5 = [|-400; 100; -900; 50; -15|]
let _ = main 0 vec5*)
	
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -50|]
let _ = main 0 vec1
let vec2 = [|20; 80; 70; 10; 30; 50; 60; 40|]
let _ = main 1 vec2
let vec2 = [|100; 80; 70; 10; 30; 50; 15; 40|]
let _ = main 3 vec2*)