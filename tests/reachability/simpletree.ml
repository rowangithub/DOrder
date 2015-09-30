type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

type 'a enumeration = End | More of 'a * 'a tree * 'a enumeration

let rec height t = match t with
	| Empty -> 0
	| Node (t1, x, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1
				
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
  else Node (l, y, n)				
				
let rec add t x =
	match t with
		| Empty -> Node (Empty, x, Empty) 
		| Node (l, x', n) ->
			if x' = x then Node (l, x, n)
			else if (x < x') then (*Node (add x l, x', n)*)
				bal x' (add l x) n
			else (*Node (l, x', add x n) *)
				bal x' l (add n x)
				
let rec elements_aux accu s = 
	match s with
		| Empty -> accu
    | Node(l, x, n) -> elements_aux (x :: elements_aux accu n) l

let elements s =
  elements_aux [] s
let _ = elements Empty		

let rec cons_enum s e =
	match s with
		| Empty -> e
    | Node(l, value, n) -> cons_enum l (More(value, n, e))