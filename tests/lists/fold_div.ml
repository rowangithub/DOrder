let rec fold_left f (acc:int) (xs:int list) =
  match xs with
      [] -> acc
    | x::xs' -> fold_left f (f acc x) xs'

(*let rec randpos n =
 let m = Random.int n in
 if m > 0 then m else randpos n*)

let rec make_list n =
  if n <= 0 then []
  else (1 + Random.int n) :: make_list (n-1)

let div x y = (assert (y <> 0); x / y)

let main n m =
  let xs = make_list n in
  fold_left div m xs
	
let _ = main 1 1