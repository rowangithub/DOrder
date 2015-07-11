	type 'a tree = 
		| Leaf 
		| Node of 'a tree * 'a * 'a tree
	
	
	let rec size tree =
    match tree with
			| Node (l, x, n) -> (size l) +(size n) + 1
      | Leaf -> 0

  let rec insert x t =
		let s = size t in
    match t with
			| Leaf -> Node (Leaf, x, Leaf)
      | Node (l, y, n) ->
	  		if s mod 2 = 0
	  		then 
	    		let newT = insert x l
	    		in 
					match newT with
						| Node (newL, newV, newR) ->
	      			if newV < y
	      			then  Node (Node (newL, y, newR), newV, n)
	      			else Node (newT, y, n)
	  		else 
	    		let newT = insert x n
	    		in 
					match newT with
						| Node (newL, newV, newR) ->
	      			if newV < y
	      			then  Node (l, newV, Node (newL, y, newR))
	      			else Node (l, y, newT)

  let findMin (phantom:bool) t : int =
    match t with
			| Node (l, x, n) -> x
      (*| Leaf -> failwith "No elements in the heap"*)

  let rec check t = 
		match t with Node (l, x, n) ->
    (match (l,n) with
			| (Leaf,Leaf) -> t
      | (Leaf, Node (rl, xr, rr)) ->
	  		if x > xr
	  		then Node (l, xr, Node (rl, x, rr))
	  		else t
      | (Node (ll, xl, lr), Leaf) -> 
	  		if x > xl
	  		then Node (Node (ll, x, lr), xl, n)
	  		else t
      | (Node (ll, xl, lr), Node (rl, xr, rr)) ->
	  		if x <= xl && x <= xr
	  		then t
	  		else if xl < xr
	  			then Node (check (Node (ll, x, lr)), xl, n)
	  			else Node (l, xr, check (Node (rl, x, rr)))
					
	)

  let rec deleteMin t = 
		let s = size t in
		match t with Node (l, x, n) ->
    (match (l, n) with
			| (Leaf, Leaf) -> (x, Leaf)
			| (Leaf, n) -> (x, n)
			| (l, Leaf) -> (x, l)
      | (a, b) ->
	  		if s mod 2 = 0
	  		then 
	    		let (newV, newL) = deleteMin l
	    		in (x, check (Node (newL, newV, n)))
	  		else
	    		let (newV, newR) = deleteMin n
	    		in (x, check (Node (l, newV, newR)))
		)

	let harness () = findMin true Leaf