let rec halve xs =
  match xs with
  | []   -> ([], [])
  | x::xs' ->
      let (ys, zs) = halve xs' in
      (x::zs, ys)	
			
let rec merge xs ys = 
	match xs with
		| [] -> ys
		| x::xs' -> (
			match ys with
				| [] -> xs
				| y::ys' -> 
					if x < y then x :: (merge xs' ys)
					else y :: (merge xs ys')
		)			

let rec mergesort xs =
  match xs with
  | [] -> []
  | x::xs' -> (
      match xs' with 
			| [] -> [x] 
      | y::ys -> 
        let (qs,rs) = halve xs in
        let qs' = mergesort qs in
        let rs' = mergesort rs in
        merge qs' rs'
    )
				
let main () = 
	mergesort [3; 2; 4; 1]
let _ = main ()