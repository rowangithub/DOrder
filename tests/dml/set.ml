type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

let rec height t = match t with
	| Empty -> 0
	| Node (t1, dv, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1
		
let height1 t = match t with
	| Empty -> 0
	| Node (t1, dv, t2) -> height t1

let height2 t = match t with
	| Empty -> 0
	| Node (t1, dv, t2) -> height t2		


let bal l x n =
  let hl = height l in
 	let hr = height n in
  if hl > hr + 2 then
		match l with
			| Node (ll, lv, lr) ->
		    if height ll >= height lr then
		      Node (ll, lv, (Node (lr, x, n)))
		    else
		      match lr with
						| Node (lrl, lrv, lrr) ->
		          Node (Node (ll, lv, lrl), lrv, (Node (lrr, x, n)))
  else if hr > hl + 2 then
		match n with
			| Node (rl, rv, rr) ->
        if height rr >= height rl then
          Node (Node (l, x, rl), rv, rr)
        else
          match rl with
            Node (rll, rlv, rlr) ->
              Node (Node (l, x, rll), rlv, Node (rlr, rv, rr)) 
  else Node (l, x, n) 
(*withtype {sl:nat,hl:nat} 'a tree(sl,hl) -> 'a ->
         {sr:nat,hr:nat} <> => 'a tree(sr,hr) -> [h':pos] 'a tree (sl+1+sr,h')*)
				
let rec add x t = 
	let t' = 
	match t with
	| Empty -> Node (Empty, x, Empty)
	| Node (l, dv, n) ->
  	if (x = dv) then t
		else if x < dv then
			bal (add x l) dv n
    else bal l dv (add x n) in
	let _ = assert (height t' >= height t && height t' <= height t + 1) in t'	
(*withtype 'a -> {s:nat,h:nat} <s> => 'a tree(s,h) ->
         [s':pos,h':pos | s <= s' <= s+1] 'a tree(s',h')*)

		
let rec remove_min_binding t = match t with
	(*| Empty -> invalid_arg "Map.remove_min_elt"*)
	| Node (l, dv, n) -> (match l with
		| Empty -> n
		| Node (l', dv', n') -> bal (remove_min_binding l) dv n 
		) 
  
	
let rec nfind x t = match t with
    Empty -> ()
  | Node(l, x, n) ->
      let xx = nfind x l in
      let yy = nfind x n in ()
			
let rec min_elt t = match t with
	(*| Empty -> raise Not_found*)
  | Node(l, dv, n) -> (match l with
		| Empty -> dv
		| Node (l', dv', n') -> min_elt l
		)		
		
let merge l n = match (l, n) with
	| (Empty, t) -> t
	| (t, Empty) -> t
  | (t1, t2) -> bal t1 (min_elt t2) (remove_min_binding t2)
	
		
let rec remove x t = 
	let t' = match t with
	| Empty -> Empty
	| Node(l, dv, n) ->
  	if x = dv then
    	merge l n
  	else if x < dv then
    	bal (remove x l) dv n
  	else
    	bal l dv (remove x n) in
	let _ = assert (height t' >= height t -1 && height t' <= height t) in t'		
			
let rec find t x = match t with
    (*Empty ->
      let _ = assert (1=0) in assert false *)
  | Node(l, d, n) ->
      if x = d then d else
        if x < d then 
          let xx = nfind x n in
          find l x
          
        else
          let xx = nfind x l in
          find n x	