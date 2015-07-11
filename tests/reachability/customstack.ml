type 'a stack = 
	| Nil 
	| Cons of 'a * 'a stack

let cons x s = Cons (x, s)

let empty = Nil

(*let is_empty s = 
	s = Nil *)

let head s = match s with
	(*| Nil -> raise Empty *)
	| Cons (x, s) -> x

let tail s = match s with
	(*| Nil -> raise Empty *)
	| Cons (x, s) -> s

let rec merge xs ys =
	match xs with
		| Nil -> ys
		| Cons (x, xs') ->
			cons x (merge xs' ys)