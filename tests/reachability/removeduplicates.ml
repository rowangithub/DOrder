let reverse l =
	let rec aux us ys = 
		match us with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
	aux l []
	
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
	(remove_duplicates_in_sorted_list [2;3;4])
	
let _ = main ()			