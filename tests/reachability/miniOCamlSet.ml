(** A mini-demo of the tree based implementation of Ocaml-Set. 
   This demo supports tree insertion, removal, membershipe query,
	 max_elt query, min_elt query and convertion to list *)

type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

type 'a enumeration = 
	| End 
	| More of 'a * 'a tree * 'a enumeration

let rec height t = match t with
	| Empty -> 0
	| Node (t1, x, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1
		
let rec cons_enum s e =
	match s with
		| Empty -> e
    | Node(l, value, n) -> 
			cons_enum l (More(value, n, e))		
				
(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)				
				
let bal y l n =
	let hl = height l in
 	let hr = height n in
  if hl > hr + 2 then
		match l with
			| Node (ll, lv, lr) ->
		    if height ll >= height lr then
		      Node (ll, lv, (Node (lr, y, n)))
		    else
		      match lr with
						| Node (lrl, lrv, lrr) ->
		          Node (Node (ll, lv, lrl), lrv, (Node (lrr, y, n)))
  else if hr > hl + 2 then
		match n with
			| Node (rl, rv, rr) ->
        if height rr >= height rl then
          Node (Node (l, y, rl), rv, rr)
        else
          match rl with
            Node (rll, rlv, rlr) ->
              Node (Node (l, y, rll), rlv, Node (rlr, rv, rr)) 
  else 
		Node (l, y, n)				
	
							
let rec add t x =
	match t with
		| Empty -> 
			Node (Empty, x, Empty) 
		| Node (l, x', n) ->
			if x' = x then Node (l, x, n)
			else if (x < x') then (*Node (add x l, x', n)*)
				bal x' (add l x) n
			else (*Node (l, x', add x n) *)
				bal x' l (add n x)
	
							
let rec remove_min_binding t = 
	match t with
		(*| Empty -> invalid_arg "Map.remove_min_elt"*)
		| Node (l, x, n) -> (
			match l with
				| Empty -> (x, n)
				| Node (l', x', n') -> 
					let (m, l'') = remove_min_binding l in 
					(m, bal x l'' n(*Node (l'', x, n)*))
			) 
	

let rec remove x tr = 
	match tr with
		| Empty -> Empty
		| Node(l, x', n) ->
  		if x' = x then
    		match (l, n) with
				| (Empty, t) -> t
				| (t, Empty) -> t
  			| (t1, t2) -> 
					let (m, t2) = remove_min_binding t2 in
					bal m t1 t2
  		else if x' > x then
    		bal x' (remove x l) n
  		else
    		bal x' l (remove x n)		
				

(* Smallest and greatest element of a set *)			
			
let rec min_elt (phantom:bool) t : int = 
	match t with
		(*| Empty -> raise Not_found*)
  	| Node(l, x, n) -> (
			match l with
				| Empty -> x
				| Node (l', x', n') -> min_elt phantom l
			)	
	
					
let rec max_elt (phantom:bool) t : int = 
	match t with
		(*| Empty -> raise Not_found *)
		| Node (l, x, n) -> (
			match n with
				| Empty -> x
				| Node (l', x', n') -> max_elt phantom n
			)


let rec mem x s = 
	match s with
		| Empty -> false
    | Node(l, y, n) ->
			if x = y then true
			else if x < y then
				mem x l
			else mem x n	
									
																																																																																																																													
let rec elements_aux accu s = 
	match s with
		| Empty -> accu
    | Node(l, x, n) -> 
			elements_aux (x :: elements_aux accu n) l


let elements s =
  elements_aux [] s	