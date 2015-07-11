let rec make_list m =
  if m <= 0
  then []
  else (m) :: make_list (m-1)

let rec make_list_list m =
  if m <= 0
  then []
  else make_list (m) :: make_list_list (m-1)

(*let ne (xs: int list) = match xs with
    [] -> 0
  | x::xs -> 1

let rec filter p (xs: int list list) = match xs with
    [] -> []
  | x::xs -> if p x = 1 then x::(filter p xs) else filter p xs

let rec ok (xs: int list list) = match xs with
    [] -> []
  | x::xs -> 
		(assert (List.length x > 0); 
		x :: ok xs)
		
let main m =
	let xs = filter ne (make_list_list m) in
	ok xs*)		
		
(*************************************************************************)		
let head xs = match xs with
    [] -> assert false
  | x::xs -> x

let ne (xs: int list) = match xs with
    [] -> 0
  | x::xs -> 1

let rec filter p (xs: int list list) = match xs with
    [] -> []
  | x::xs -> if p x = 1 then x::(filter p xs) else filter p xs

let rec map f (xs: int list list) = match xs with
    [] -> []
  | x::xs -> f x :: map f xs

let main m = map head (filter ne (make_list_list m))		
(*************************************************************************)		
	
let _ = main 3