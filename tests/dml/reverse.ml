let reverse l =
	let rec aux xs ys = 
		match xs with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
    aux l []
		
let main () = 
	let l = [1; 2; 3; 4; 5] in
	let ll = reverse l in
	assert (List.length ll = List.length l)
		
let _ = main ()		
