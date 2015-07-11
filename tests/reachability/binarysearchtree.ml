type 'a tree = 
	| E 
	| T of 'a tree * 'a * 'a tree
	
let empty = E

let rec member z t =
	match t with
	| E -> false
	| T (a, y, b) ->
	    if z < y then member z a
	    else if y < z then member z b
	    else true 

let rec insert x t = 
	match t with
    | E -> T (E, x, E)
    | T (a, y, b) ->
        if x < y then T (insert x a, y, b)
        else if y < x then T (a, y, insert x b)
        else t
				
let rec set_of_list xs = 
	match xs with
		| [] -> E
   	| x :: l -> insert x (set_of_list l)

let rec tree_max (fantom: bool) t : int =
	match t with
		| T (a,x,E) -> x
		| T (a,x,rt) -> tree_max fantom rt

let rec delete n t =
	match t with
		| E -> E
		| T (lt,x,rt) ->
			if x = n then
				match (lt, rt) with
					| (E, E) -> E
					| (T (a,b,c) , E) -> lt
					| (E, T (a,b,c)) -> rt
					| (T (a,b,c), T (a',b',c')) -> 
						let m = tree_max true lt in
					  T (delete m lt, m, rt)
			else if n < x then T(delete n lt, x, rt)
			else T (lt, x, delete n rt)

let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys

let rec pre_order t =
	match t with
		| E -> []
		| T (lt,x,rt) -> x::(append (pre_order lt) (pre_order rt)) 

let rec in_order t = 
	match t with
  	| E -> []
  	| T (lt,x,rt) -> append (in_order lt) (x :: in_order rt)

(*let rec level_order_aux l =
	match l with
		| [] -> []
		| t::rest -> 
			match t with
				| E -> level_order_aux rest
				| T (lt,x,rt) -> x::(level_order_aux (append rest [lt;rt]))

let rec level_order t =
	 level_order_aux [t]*)