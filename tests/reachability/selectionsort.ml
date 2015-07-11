let rec getmin xs : int =
	match xs with
		| x::xs' -> (
			match xs' with 
				| [] -> x
				| y::ys ->
					let m = getmin xs' in
					if (x < m) then x
					else m) 
					
let rec rem_min m xs = 
	match xs with
		| x :: xs ->
			if (x = m) then xs
			else x :: (rem_min m xs)

let rec selection_sort xs = 
	match xs with
		| [] -> []
		| x::xs' ->
			let m = getmin xs in
			m::(selection_sort (rem_min m xs))		
			
let main () = 
	selection_sort [3; 2; 4; 5; 7; 6]
let _ = main () 