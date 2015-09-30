let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys

let main () = append [1;9;3] [4;5;2;6]
let _ = main ()