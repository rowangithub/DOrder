let reverse l =
	let rec aux us ys = 
		match us with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
    aux l []
		
let main () = 
	let t = [1;3;2;5;4] in
	let _ = reverse t in
	()
let _ = main ()