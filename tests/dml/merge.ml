let rec merge xs ys = 
	match xs with
		| [] -> ys
		| x::xs' -> (
			match ys with
				| [] -> xs
				| y::ys' -> 
					if x < y then x :: (merge xs' ys)
					else y :: (merge xs ys')
		)
		
let main () = 
	let l1 = [1; 2] in
	let l2 = [3; 4; 5] in
	let l = merge l1 l2 in
	assert (List.length l1 + List.length l2 = List.length l)
	
let _ = main ()