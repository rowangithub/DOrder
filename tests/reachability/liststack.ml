let empty = []

(*let is_empty s = 
	s = [] *)

let cons x s = x :: s

let head s =
	match s with
		(*| [] -> raise Empty *)
		| h :: t -> h

let tail s = 
	match s with
		(*| [] -> raise Empty *)
		| h :: t -> t

(*let (++) = (@) *)
let rec merge xs ys = 
	match xs with
  | [] -> ys
  | xh :: xt -> xh :: (merge xt ys)

let rec update lst i y = 
	match lst with
  (*| [] -> raise Subscript*)
  | x :: xs ->
		if (i = 0) then y :: xs
		else x :: (update xs (i - 1) y)
		
let main () = ()
let _ = main ()