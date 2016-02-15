(*let empty = [], []
 
let is_empty (f, _) = f = [] *)

let reverse l =
	let rec aux us ys = 
		match us with
			| [] -> ys
			| x::zs -> aux zs (x::ys) in
    aux l []

let checkf ((front, rear) : ('a list * 'a list)) = 
	match front with
		| [] -> (reverse rear, front)
		| x::front' -> (front, rear)

let snoc ((front, rear) : ('a list * 'a list)) x = 
	checkf (front, x :: rear)

let head ((front, rear) : ('a list * 'a list)) = 
	match (front, rear) with
	(*| [], rear -> raise Empty *)
	| x :: front', rear -> x

let tail ((front, rear) : ('a list * 'a list)) = 
	match (front, rear) with
		(*| [], _ -> raise Empty *)
		| x :: front', rear -> checkf (front', rear)

let main () = ()
let _ = main ()