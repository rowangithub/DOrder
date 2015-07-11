let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys

let main () = append [1;3;2] [4;5;9;6]
let _ = main ()