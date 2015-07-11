let rec partition (i:int) (j:int) (p:int) (q:int) (x:int) (a:int array) =
	if (j < q) then
		if (a.(j) <= x) then
			let i = i + 1 in
			let tmp = a.(i) in
			let _ = a.(i) <- a.(j) in
			let _ = a.(j) <- tmp in
			partition i (j+1) p q x a
		else if (a.(j) > x) then partition i (j+1) p q x a
		else i+1
	else i+1

let rec quicksort p q (a:int array) = 
	if (p < q) then
		let x = a.(q-1) in
		let m = partition (p-1) p p q x a in
		let _ = quicksort p (m-1) a in
		let _ = quicksort m q a in ()
	else ()

let main (vec:int array) =
	(let n = Array.length vec in
	quicksort 0 n vec)	
	
let vec1 = [|90; -5; -30; -10;|]
let _ = main vec1	
		
(*let vec1 = [|200; -300; 100; -500; 400; -100; 900; -50|]
let _ = main (*0 (Array.length vec1)*) vec1
let vec2 = [|20; 80; 70; 10; 30; 50; 60; 40|]
let _ = main (*1 (Array.length vec2)*) vec2
let vec2 = [|100; 80; 70; 10; 30; 50; 15; 40|]
let _ = main (*3 (Array.length vec2)*) vec2*)