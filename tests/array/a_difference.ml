let rec diff i (n:int) (a: int array) (b: int array) (c : int array) =
	if (i < n) then
		let _ = c.(i) <- (a.(i) - b.(i)) in
		diff (i+1) n a b c
	else ()
	
let main a b = 
	let n = Array.length a in
	let c = Array.make n 0 in
	diff 0 n a b c
	
let vec1 = [|-100; -200; -300; 350; 370|]
let vec2 = [|-200; -400; -500; 150; 70|]
let _ = main vec1 vec2