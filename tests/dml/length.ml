let rec length xs = 
	match xs with
		| [] -> 0
		| x :: xs -> 1 + length xs
(*withtype {n:nat} <n> => 'a list(n) -> int(n)*)

let main () = 
	let l = [1; 2; 3; 4; 5] in
	assert (length l = List.length l)
	
let _ = main ()