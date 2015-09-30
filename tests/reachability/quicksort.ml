let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys
		
let rec par x rest l g = 
	match rest with
  | [] -> (l, g)
  | y :: rest' ->
      if x <= y then par x rest' l (y :: g)
      else par x rest' (y :: l) g
			
let rec qs xs = 
	match xs with 
	| [] -> []
  | x :: rest -> 
    let (l, g) = par x rest [] [] in
    (*(qs l) @ (x :: (qs g))			*)
		append (qs l) (x::(qs g))
		
let main () = ()
let _ = main ()