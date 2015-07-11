let rec zip xs ys = 
	match xs with
		| [] -> (match ys with
			| [] -> []
			| y::ys -> assert false)
		| x::xs -> (match ys with
			| [] -> assert false
			| y::ys -> (x, y) :: (zip xs ys))

(*let rec zip xs ys = 
	match (xs, ys) with
		| ([], []) -> []
		| (x::xs, y::ys) -> (x, y) :: (zip xs ys)*)

let main () = 
	let l1 = [1; 2; 3] in
	let l2 = [4; 5; 6] in
	let l = zip l1 l2 in
	assert (List.length l1 = List.length l)
	
let _ = main ()