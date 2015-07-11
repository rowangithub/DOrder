(*let insert_sort t =
	let length = Array.length t in
 	for start = 1 to length - 1 do
 		let pos = ref start in
 		while !pos > 0 && t.(!pos - 1) > t.(!pos) do
 			swap (!pos-1) (!pos) t;
 			decr pos
 		done
 done*)

let rec insert i (j:int) (key:int) l =
	if (i >= 0) then
		if (Array.get l i > key) then
			(let _ = Array.set l (i+1) (Array.get l i) in
			insert (i-1) j key l)
		else 
			Array.set l (i+1) key
	else Array.set l (i+1) key 
			
(*let rec sort j n l = 
	if j < n then
		let key = Array.get l j in
		(let _ = insert (j-1) j key l in
		sort (j+1) n l)
	else () *)
	(*if (n <= 1) then ()
	else 
		let key = Array.get l (n-1) in
		(sort l (n-1);
		insert (n-2) (n-1) key l)*)
	
let main vec i = 
	let n = Array.length vec in
	let key = Array.get vec i in 
	let _ = insert (i-1) i key vec in
	assert (Array.get vec i >= key)
	(*(let n = Array.length vec in
	let _ = sort 1 n vec in
	if (0 < ith && ith < n) then
		assert (Array.get vec (ith-1) <= Array.get vec ith)
	else ())*)


let vec1 = [|200; -300; 100; -500; 400|]
let vec2 = [|-1000; 2000; -3000; 4000; -5000|]
let vec3 = [|-100; 900; -800; 100; 200|]
let _ = main vec1 3
let _ = main vec2 3
let _ = main vec3 3
let _ = main vec1 4
let _ = main vec2 4
let _ = main vec3 4
let _ = main vec1 2
let _ = main vec2 2
let _ = main vec3 2
 
