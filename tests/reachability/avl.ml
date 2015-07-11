(* An implementation of AVL trees in DML *)

type 'a avltree = 
	| E
	| Bl of 'a * 'a avltree * 'a avltree (* rh <= lh <= rh+1 *)
	| Br of 'a * 'a avltree * 'a avltree (* lh <= rh <= lh+1 *)


let rec height t = match t with
	| E -> 0
	| Bl (x, t1, t2) -> 1 + height t1
	| Br (x, t1, t2) -> 1 + height t2


let left_rotate e xl xn = 
	match xl with
	| Bl (le, ll, lr) ->
	    let lrh = height lr
	    in Br (le, ll, Bl (e, lr, xn)) (* height = lrh + 2 *)
	| Br (le, ll, lr) ->
	    let llh = height ll in
			let lrh = height lr in
	   	if llh < lrh then (* rh = llh: deep rotation *)
		  	match lr with
				| Bl (lre, lrl, lrr) -> Br (lre, Bl (le, ll, lrl), Br (e, lrr, xn))
				| Br (lre, lrl, lrr) -> Br (lre, Bl (le, ll, lrl), Br (e, lrr, xn))
			else Br (le, ll, Bl (e, lr, xn))


let right_rotate e yl yn = 
	match yn with
	| Bl (re, rl, rr) ->
			let rlh = height rl in
			let rrh = height rr in
	    if rlh > rrh then (* lh = rrh: deep rotation *)
		  	match rl with
					| Bl (rle, rll, rlr) -> Bl (rle, Bl (e, yl, rll), Br (re, rlr, rr))
		      | Br (rle, rll, rlr) -> Bl (rle, Bl (e, yl, rll), Br (re, rlr, rr))
			else Bl (re, Br (e, yl, rl), rr) (* height = rlh+2 *)
	| Br (re, rl, rr) ->
	    let rlh = height (rl) in
			Bl (re, Br (e, yl, rl), rr) (* height = rlh+2 *)

let rec insert x t = 
	match t with
	| E -> Bl (x, E, E) (* height = 1 *)
  | Bl (e', l, n) ->
    if (x < e') then
			let l' = insert x l in
		 	let lh' = height l' in
		 	let rh = height n in
	    if lh' <= rh then Br (e', l', n) (* height = rh + 1 *)
		 	else if lh' <= rh+1 then Bl (e', l', n) (* height = lh' + 1 *)
		  else left_rotate e' l' n
		else if (x > e') then
			let lh = height l in
		 	let n' = insert x n in
		 	let rh' = height n' in
		 	if rh' <= lh then Bl (e', l, n') (* height = lh + 1 *)
		 	else Br (e', l, n') (* height = rh' + 1 *)
		else t
	| Br (e', l, n) ->
		if (x < e') then
			let l' = insert x l in
		 	let lh' = height l' in
		 	let rh = height n in
		 	if lh' <= rh then Br (e', l', n) (* height = rh + 1 *)
		 	else Bl (e', l', n) (* height = lh' + 1 *)
		else if (x > e') then 
			let lh = height l in
		 	let n' = insert x n in
		 	let rh' = height n' in
		 	if rh' <= lh then Bl (e', l, n') (* height = lh + 1 *)
		 	else if rh' <= lh+1 then Br (e', l, n') (* height = rh' + 1 *)
		  else right_rotate e' l n' 
		else t 