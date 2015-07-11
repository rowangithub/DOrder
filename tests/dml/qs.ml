let rec par x rest l g = 
	match rest with
    [] -> (l, g)
  | y :: rest ->
      if x <= y then par x rest l (y :: g)
      else par x rest (y :: l) g
(*withtype
  {n:nat,p:nat,q:nat} <n> =>
    int * int list(n) * int list(p) * int list(q) ->
    [p':nat,q':nat | p'+q'=n+p+q] (int list(p') * int list(q'))*)

let rec qs xs = match xs with 
	| [] -> []
  | x :: rest -> 
    let (l, g) = par x rest [] [] in
    (qs l) @ (x :: (qs g))
(*withtype {n:nat} <n> => int list(n) -> int list(n)*)

let main () = 
	let l = [30; 20; 50; 10; 40] in
	let ll = qs l in
	assert (List.length l = List.length ll)
		
let _ = main ()		