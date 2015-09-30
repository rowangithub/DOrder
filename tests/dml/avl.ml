(* An implementation of AVL trees in DML *)

type 'a avltree = 
	| E
	| Bl of 'a * 'a avltree * 'a avltree (* rh <= lh <= rh+1 *)
	| Br of 'a * 'a avltree * 'a avltree (* lh <= rh <= lh+1 *)

let rec size t = match t with
	| E -> 0
	| Bl (x, t1, t2) -> 1 + size t1 + size t2
	| Br (x, t1, t2) -> 1 + size t1 + size t2 

let rec height t = match t with
	| E -> 0
	| Bl (x, t1, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1	
	| Br (x, t1, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1	

let rec treebal tree = match tree with
	| E -> 1
	| Bl (x, t1, t2) -> 
		if height t1 >= height t2 && height t1 <= height t2 + 1 
				&& treebal t1 = 1 && treebal t2 = 1 then 1
		else 0
	| Br (x, t1, t2) ->
		if height t2 >= height t1 && height t2 <= height t1 + 1
				&& treebal t1 = 1 && treebal t2 = 1 then 1
		else 0 


let left_rotate e (l: 'a avltree) (n: 'a avltree) = 
	match l with
	| Bl (le, ll, lr) ->
	    let lrh = height lr
	    in Br (le, ll, Bl (e, lr, n)) (* height = lrh + 2 *)
	| Br (le, ll, lr) ->
	    let llh = height ll in
			let lrh = height lr in
	   	if llh < lrh then (* rh = llh: deep rotation *)
		  	match lr with
				| Bl (lre, lrl, lrr) -> Br (lre, Bl (le, ll, lrl), Br (e, lrr, n))
				| Br (lre, lrl, lrr) -> Br (lre, Bl (le, ll, lrl), Br (e, lrr, n))
			else Br (le, ll, Bl (e, lr, n))
(*withtype {ls:nat,rh:nat,rs:nat} <> =>
         'a * 'a avltree(rh+2, ls) * 'a avltree(rh, rs) ->
	 [h':nat | rh+2 <= h' <= rh+3] 'a avltree (h',1+ls+rs)*)

let right_rotate e (l : 'a avltree) (n: 'a avltree) = 
	match n with
	| Bl (re, rl, rr) ->
			let rlh = height rl in
			let rrh = height rr in
	    if rlh > rrh then (* lh = rrh: deep rotation *)
		  	match rl with
					| Bl (rle, rll, rlr) -> Bl (rle, Bl (e, l, rll), Br (re, rlr, rr))
		      | Br (rle, rll, rlr) -> Bl (rle, Bl (e, l, rll), Br (re, rlr, rr))
			else Bl (re, Br (e, l, rl), rr) (* height = rlh+2 *)
	| Br (re, rl, rr) ->
	    let rlh = height (rl) in
			Bl (re, Br (e, l, rl), rr) (* height = rlh+2 *)
(*withtype {lh:nat,ls:nat,rs:nat} <> =>
         'a * 'a avltree(lh, ls) * 'a avltree(lh+2, rs) ->
	 [h':nat | lh+2 <= h' <= lh+3] 'a avltree (h',1+ls+rs)*)


let rec insert e t = 
	let t' = match t with
	| E -> Bl (e, E, E) (* height = 1 *)
  | Bl (e', l, n) ->
    if (e < e') then
			let l' = insert e l in
		 	let lh' = height l' in
		 	let rh = height n in
	    if lh' <= rh then Br (e', l', n) (* height = rh + 1 *)
		 	else if lh' <= rh+1 then Bl (e', l', n) (* height = lh' + 1 *)
		  else left_rotate e' l' n
		else if (e > e') then
			let lh = height l in
		 	let n' = insert e n in
		 	let rh' = height n' in
		 	if rh' <= lh then Bl (e', l, n') (* height = lh + 1 *)
		 	else Br (e', l, n') (* height = rh' + 1 *)
		else t
	| Br (e', l, n) ->
		if (e < e') then
			let l' = insert e l in
		 	let lh' = height l' in
		 	let rh = height n in
		 	if lh' <= rh then Br (e', l', n) (* height = rh + 1 *)
		 	else Bl (e', l', n) (* height = lh' + 1 *)
		else if (e > e') then 
			let lhttt = height l in
		 	let n' = insert e n in
		 	let rh' = height n' in
		 	if rh' <= lhttt then Bl (e', l, n') (* height = lh + 1 *)
		 	else if rh' <= lhttt+1 then Br (e', l, n') (* height = rh' + 1 *)
		  else right_rotate e' l n' 
		else t in
	let _ = assert (height t' >= height t) in
	let _ = assert (height t' <= height t + 1) in
	t'
(*withtype {h:nat,s:nat} <s> => 'a * 'a avltree(h,s) ->
         [h':nat | h <= h' <= h+1] 'a avltree(h',s+1)*)