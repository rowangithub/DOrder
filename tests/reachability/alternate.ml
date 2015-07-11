let rec alternate xs ys = 
	match xs with
		| [] -> []
		| x::xs' -> 
			x::(alternate ys xs') 
			
let main () = 
	(alternate [3;2;1;7] [4;6;5;0;10;9;];
	alternate [4;6;5;0;10;9] [3;2;1];
	alternate [3;2;1] [4;6;5]
	)
let _ = main () 