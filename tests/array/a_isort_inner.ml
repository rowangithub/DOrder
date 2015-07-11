let rec insert i (j:int) (key:int) l =
	if (i >= 0) then
		if (Array.get l i > key) then
			(let _ = Array.set l (i+1) (Array.get l i) in
			insert (i-1) j key l)
		else ()
	else ()
	

let main vec i = 
	let n = Array.length vec in
	let key = Array.get vec i in 
	insert (i-1) i key vec
	
(*let vec1 = [|200; -300; 100; -500; 400|]
let vec2 = [|-1000; 2000; -3000; 4000; -5000|]
let vec3 = [|-100; 900; -800; 100; 200|]
let _ = main vec1 3
let _ = main vec2 3
let _ = main vec3 3*)

let vec1 = [|200; -300; -100; 900; -50|]
let _ = main vec1 4

let vec3 = [|100; 80; 30; 50; 40|]
let _ = main vec3 2