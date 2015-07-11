let reverse l =
	let rec aux us ys = 
		match us with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
    aux l []

let rec shuffle xs = 
	match xs with
		| [] -> []
		| x' :: xs' ->
			let xs = reverse xs in
			match xs with
				| x' :: xs' -> x' :: (shuffle xs')

let main () = 
	shuffle [5; 3; 4; 6; 2; 9; 7]
let _ = main ()