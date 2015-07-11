let rec append xs ys = 
	match xs with
		| [] -> ys
		| x::xs -> x::(append xs ys)
(*withtype {m:nat, n:nat} 'a list(m)*'a list(n)->'a list(m+n)*)

let main () = 
	let l1 = [1; 2] in
	let l2 = [3; 4; 5] in
	let l = append l1 l2 in
	assert (List.length l1 + List.length l2 = List.length l)
	
let _ = main ()