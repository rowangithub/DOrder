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


let main p q (a:int array) =
	let x = a.(q-1) in
	let _ = partition (p-1) p p q x a in
	()
	
let vec1 = [|200; -300; -100; 900; -50|]
let _ = main 0 (Array.length vec1) vec1

let vec3 = [|100; 80; 30; 50; 40|]
let _ = main 3 (Array.length vec3) vec3