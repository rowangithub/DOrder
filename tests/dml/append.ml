let rec append xxs yys =
	match xxs with
		| [] -> yys
		| xs::xxs -> xs :: (append xxs yys)
(*withtype {m:nat,n:nat} <m> =>
         'a list(m) -> 'a list(n) -> 'a list(m+n)*)

let rec reverse xs = 
	match xs with
		| [] -> []
		| x::xs -> append (reverse xs) [x]
(*withtype {n:nat} <n> => 'a list(n) -> 'a list(n)*)

let main () = 
	let l = [1; 2; 3; 4; 5] in
	let ll = reverse l in
	assert (List.length ll = List.length l)
	
let _ = main ()