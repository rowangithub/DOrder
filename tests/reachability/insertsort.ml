let rec insert y xs = 
	match xs with
		| [] -> [y]
		| x::xs' ->
			if y <= x then y::xs
			else x :: (insert y xs') 
			
let rec insertsort xs = 
	match xs with
		| [] -> []
		| x::xs' -> 
			insert x (insertsort xs')		
			
let main () = 
	insertsort [3; 2; 4;]
let _ = main ()