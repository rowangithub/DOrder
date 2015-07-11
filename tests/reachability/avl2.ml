 	type 'a tree = 
		| Leaf 
		| Node of int * 'a tree * 'a * 'a tree

	let max (a:int) (b:int) = 
		if a >= b then a
		else b

  let rec member e t = 
    match t with
			| Leaf -> false
			| Node (h,l,x,n) ->
				if e < x then
					member e l
				else if e = x then
					true
				else member e n
				

  let depth tree = 
    match tree with
			| Node (d, l, x, n) -> d
      | Leaf -> 0

  let balanceLL node = 
    match node with 
		(Node (d, Node (lmax, ll, xl, rl), x, n)) ->
	  	let rmax = max (depth rl) (depth n) + 1 in 
			let cmax = max rmax (depth ll) + 1
	  	in Node (cmax, ll, xl, Node (rmax, rl, x, n))
    (*| _ -> failwith "Impossible"*)

  let balanceLR node =
    match node with 
		(Node (d, Node (dl, ll, y, Node (dlr, lrl, z, lrr)), x, n)) -> 
	  	let lmax = max (depth ll) (depth lrl) + 1 in
			let rmax = max (depth lrr) (depth n) + 1 in 
			let cmax = max lmax rmax + 1 in 
			Node (cmax, Node (lmax, ll, y, lrl), z, Node (rmax, lrr, x, n))
    (*| _ -> failwith "Impossible"*)

  let balanceRR node = 
		match node with
    (Node (d, l, x, Node (dr, lr, xr, rr))) ->
			let lmax = max (depth l) (depth lr) + 1 in 
			let cmax = max lmax (depth rr) + 1 in 
			Node (cmax, Node (lmax, l, x, lr), xr, rr)
    (*| _ -> failwith "Impossible"*)

  let balanceRL node =
    match node with 
		(Node (d, l, x, Node (dr, Node (drl, rll, z, rlr), y, rr))) ->
	  	let lmax = max (depth l) (depth rll) + 1 in 
			let rmax = max (depth rlr) (depth rr) + 1 in 
			let cmax = max lmax rmax + 1 in 
			Node (cmax, Node (lmax, l, x, rll), z, Node (rmax, rlr, y, rr)) 
    (*| _ -> failwith "Impossible"*)

  let rec insert t e = 
    match t with
		| Node (h, l, x, n) ->
		  (if e < x then
				 let insL = insert l e
				 in let dl = depth insL
				 in let dr = depth n
				 in let bal = dl - dr
				 in
				   if bal <> 2
				   then Node ((max dr dl) + 1, insL, x, n)
				   else (
						match l with
							| Node (lh, ll, lx, ln) ->
								if e < lx then
									balanceLL (Node (dl + 1, insL, x, n))
								else if e > lx then
									balanceLR (Node (dl + 1, insL, x, n))
								else t
						)
					 (*else if e < value l
				   then balanceLL (Node (dl + 1, insL, x, n))
				   else if e > value l
				   then balanceLR (Node (dl + 1, insL, x, n))
				   else t*)
		   else if e = x then t
		   else
				 let insR = insert n e
				 in let dr = depth insR
				 in let dl = depth l
				 in let bal = dl - dr
				 in
				   if bal <> -2
				   then Node ((max dr dl) + 1, l, x, insR)
				   else (
						match n with
							| Node (nh, nl, nx, nn) ->
								if e > nx then
									balanceRR (Node (dr + 1, l, x, insR))
								else if e < nx then
									balanceRL (Node (dr + 1, l, x, insR))
								else t
						)
					 (*else if e > value n
				   then balanceRR (Node (dr + 1, l, x, insR))
				   else if e < value n
				   then balanceRL (Node (dr + 1, l, x, insR))
				   else t*)
					)
    | Leaf -> Node (1, Leaf, e, Leaf)

	

  let rec min (phantom:bool) tree : int = 
    match tree with
			| Node (h, l, x, n) -> 
				(match l with
					| Leaf -> x
					| Node (lh, ll, lx, ln) -> min phantom ll)
			(*| Leaf _ -> failwith "Impossible"*)
      
  
  let depth_left tree = 
    match tree with
			| Node (h, l, x, n) -> depth l
      (*| Leaf -> failwith "Impossible"  *)

  let depth_rigth tree =
    match tree with
			| Node (h, l, x, n) -> depth n
      (*| Leaf -> failwith "Impossible"*)

  let rec delete e t = 
   match t with
		Node (h, l, x, n) ->
	  (if e = x then
				 (match (l, n) with
				    | (Leaf, Leaf) -> Leaf
				    | (Leaf, n) -> n
				    | (l, Leaf) -> l
				    | (l, n) -> 
					let m = min true n
					in let del = delete m n
					in let bal = depth l - depth del
					in if bal < 2
					  then Node ((max (depth l) (depth del)) + 1, l, m, del)
					  else 
					    let balL = depth_left l - depth_rigth l
					    in if balL > 0
					      then balanceLL (Node (((max (depth l) (depth del)) + 1, l, m, del)))
					      else balanceLR (Node (((max (depth l) (depth del)) + 1, l, m, del))))
	  else if e < x then 
				 let delL = delete e l
				 in let dl = depth delL
				 in let dr = depth n
				 in let bal = dl - dr
				 in
				   if bal < 2 && bal > -2
				   then Node ((max dl dr) + 1, delL, x, n)
				   else 
				     let balR = depth_left n - depth_rigth n
				     in if balR < 0
				       then balanceRR (Node ((max dl dr) + 1, delL, x, n))
				       else balanceRL (Node ((max dl dr) + 1, delL, x, n))
	   else
				 let delR = delete e n
				 in let dl = depth l
				 in let dr = depth delR
				 in let bal = dl - dr
				 in
				   if bal < 2 && bal > -2
				   then Node ((max dl dr) + 1, l, x, delR)
				   else
				     let balL = depth_left l - depth_rigth l
				     in if balL > 0
				       then balanceLL (Node ((max dl dr) + 1, l, x, delR))
				       else balanceLR (Node ((max dl dr) + 1, l, x, delR)))
      | Leaf -> Leaf