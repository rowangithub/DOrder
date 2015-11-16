	type 'a t =
    | Leaf
    | Node of 'a t * 'a * 'a t

	let rec insert xx t = 
		match t with
    | Leaf ->
        Node (Leaf, xx, Leaf)
    | Node (l, y, n) ->
        if xx <= y then
          Node (insert y n, xx, l)
        else
          Node (insert xx n, y, l)

	let rec extract t = 
		match t with
    (*| Leaf ->
        assert false*)
    | Node (Leaf, y, Leaf) ->
        (*assert (n = Leaf);*)
        y, Leaf
    | Node (l, y, n) ->
        let x, l = extract l in
        x, Node (n, y, l)	
							
	let rec replace_min x tr = 
		match tr with
			(*| Leaf -> assert false*)
			| Node (l, value, n) ->
				(value, match l, n with
					| Leaf, Leaf -> Node (l, x, n)
					| Leaf, Node (nl, nx, nn) -> 
						if x <= nx then 
							Node (l, x, n)
						else 
							let value, n' = replace_min x n in
							Node (l, nx, n')
					| Node (ll, lx, ln), Leaf -> 
						if x <= lx then
							Node (l, x, n)
						else 
							let value, l' = replace_min x l in
							Node (l', lx, n)
					| Node (ll, lx, ln), Node (nl, nx, nn) ->
						if x <= lx && x <= nx then
							Node (l, x, n)
						else if lx <= nx then
							let value, l' = replace_min x l in
							Node (l', lx, n)
						else
							let value, n' = replace_min x n in
							Node (l, nx, n')
				)
		
		
	(* merges two Braun trees [l] and [r],
 	with the assumption that [size r <= size l <= size r + 1] *)
  let rec merge l n = match l, n with
    | l, Leaf -> l
    | Node (ll, lx, lr), Node (nl, nx, nn) ->
        if lx <= nx then
          Node (n, lx, merge ll lr)
        else
          let x, l = extract l in
					let value, n' = replace_min x n in
          Node (n', nx, l)
    (*| Leaf, _ ->
        assert false (* contradicts the assumption *) *)

  let extract_min tx =
		match tx with 
    (*| Leaf ->
        raise Empty *)
    | Node (l, x, r) ->
        x, merge l r
	let harness() = extract_min Leaf 
    
