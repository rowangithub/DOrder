let rec alternate xs ys = 
	match xs with
		| [] -> ys
		| x::xs' -> 
			x::(alternate ys xs') 
			
let main () = 
	(alternate [3;2;1] [4;6;5]
	)
let _ = main () 