  type 'a tree =
    Empty
  | Node of 'a tree * 'a * 'a tree
  
  (* Representation invariant: given a node (L, V, R),
   * All values in the left subtree L are less than V, and
   * all values in the right subtree R are greater than V, and
   * both L and R also satisfy the RI.
   *)
  
	(*type set = int * (tree ref)*)
  (* Representation invariant: size is the number of elements in
   * the referenced tree. *)

  (*let empty () = (0, ref Empty) *)

  (* splay(n,k) is a BST node n' where n' contains
   * all the elements that n does, and if an
   * element keyed by k is in under n, #value n'
   * is that element.  Requires: n satisfies the
   * BST invariant.
   *)
  let rec splay (k:int) t =
		match t with
			| Node (l, value, n) -> 
			if k = value then 
				(l, value, n)
			else if k < value then
				match l with
					| Empty -> 
						(l, value, n) (* not found *)
					| Node (ll, lv, ln) -> 
						(if k = lv then 
							(ll, lv, Node (ln, value, n)) (* 1: zig *)
						else if k < lv then 
							(match ll with
								| Empty -> 
									(Empty, lv, Node (ln, value, n)) (* not found *)
								| Node (x, y, z) -> (* 2: zig-zig *)
									let (lll, llv, lln) = splay k ll in
									(lll,llv,Node (lln,lv,Node(ln,value,n)))
								)
						else
							(match ln with
								| Empty -> 
									(ll, lv, Node (Empty, value, n))
								| Node (x, y, z) -> (* 3: zig-zag *)
									let (lnl, lnv, lnn) = splay k ln in
	                (Node(ll,lv,lnl),lnv,Node(lnn,value,n)) 
								)
							)
				else 
					match n with
						| Empty -> 
							(l, value, n) (* not found *)
						| Node (nl, nv, nn) -> 
							if k = nv then 
								(Node (l,value,nl),nv,nn) (* 1: zag *)
							else if k > nv then
								(match nn with
									| Empty -> 
										(Node(l,value,nl),nv,nn) (* not found *)
									| Node (x, y, z) -> (* 3: zag-zag *)
										let (nnl, nnv, nnn) = splay k nn in
										(Node (Node (l,value,nl),nv,nnl),nnv,nnn)
										)
							else
								(match nl with
									| Empty -> 
										(Node (l,value,nl),nv,nn) (* not found *)
									| Node (x, y, z) -> (* 2: zag-zig *)
										let (nll, nlv, nln) = splay k nl in
										(Node (l,value,nll),nlv,Node(nln,nv,nn))
									)					
		
				
	let lookup t k =
		match t with
			| Empty -> 
				false
			| Node (x, y, z) ->
				let (l, value, n) = splay k t in
				if k = value then true
				else false
	
	
	let delete x tr =
    match tr with
			| Empty -> 
				Empty
      | Node (a, b, c) ->
	  		let (l, y, n) = splay x tr in
	  		if (y = x) then
					match l, n with
						| Empty, n -> n
						| l, Empty -> l
						| Node (la, lb, lc), Node (na, nb, nc) ->
							let newL, newV, newR = splay x l in
							(match newR with
								| Empty ->
									Node (newL, newV, n)
							)
				else 
					Node (l, y, n)	
	
	
	let rec add_tree x t' = 
		match t' with
			| Empty -> 
				Node (Empty, x, Empty)
			| Node (l, value, n) ->
				if value = x then 
					Node (l, value, n)
				else if value > x then
					let n' = add_tree x l in
					(Node (n', value, n))
				else 
					let n' = add_tree x n in
					(Node (l, value, n')) 
				
	
	let add x t = 
		let tree = add_tree x t in
		match tree with
			| Node (l, value, n) -> 
				let (l, value, n) = splay x tree in
				Node (l, value, n)	
	
	let harness2 () = (
		let t = Node (Empty, 0, Empty) in
		add 0 t;
		delete 0 t;
		lookup Empty 0)	