(** Implementation 1 *)	

let rec go m xs ys = match xs with
  | [] -> reverse ys
	| x::xs' -> 
	 	if (m = x) then go m xs' ys
		else go m xs' (x::ys)

let remove_elt m xs = go m xs []		

let rec go' xs ys = match xs with
  | [] -> reverse ys
  | x :: xs' -> go' (remove_elt x xs') (x::ys)

let remove_duplicates xs = go' xs []

(** Implementation 2 *)	
	
let rec go'' xs ys = 
	match xs with
    | [] -> reverse ys
		| x::xs' -> (
			match xs' with
				| [] -> reverse (x::ys)
				| x'::xs'' ->
					if (x = x') then go'' (x'::xs'') ys
					else go'' (x'::xs'') (x::ys)
			)
			
let remove_duplicates_in_sorted_list xs = 
	go'' xs []			
			
let main () = 
	(remove_duplicates [2; 2; 1]
	remove_duplicates_in_sorted_list [2;2;3] 	)				