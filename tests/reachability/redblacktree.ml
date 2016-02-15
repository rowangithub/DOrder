	type color = 
		| Red 
		| Black
	
	type 'a t = 
		| Empty 
		| Node of color * 'a t * 'a * 'a t 

	type 'a enum =
    | End
    | More of 'a  * 'a t * 'a enum

  let rec enum s e =
    match s with
      | Empty -> e
      | Node (Black, l, x, n) -> enum l (More (x, n, e))
			| Node (Red, l, x, n) -> enum l (More (x, n, e))

  (* Invariants: (1) a red node has no red son, and (2) any path from the
     root to a leaf has the same number of black nodes *)

  (* Note the use of two constructors [Black] and [Red] to save space
     (resulting in longer code at a few places, e.g. in function [remove]).
     These red-black trees saves 20\% of space w.n.t Ocaml's AVL, which
     store the height into a fourth argument. *)

  (*s Implementation of the [empty], [is_empty], [mem]
      and [singleton] are trivial. *)

  let empty = Empty

  let is_empty t = 
		match t with
			| Empty -> true 
			| _ -> false

  let rec mem x t = 
		match t with
    | Empty -> false
    | Node (c, l, dv, n) ->
			if x = dv then true
			else if x < dv then
				mem x l
			else 
				mem x n

  let singleton x = 
		Node (Black, Empty, x, Empty)

  (*s Insertion *)

  let lbalance x2 x1 x3 = 
		match x1, x2, x3 with
    | Node (Red, Node (Red,a,x,b), y, c), z, d ->
        Node (Red, Node (Black,a,x,b), y, Node (Black,c,z,d))
    | Node (Red, a, x, Node (Red,b,y,c)), z, d ->
        Node (Red, Node (Black,a,x,b), y, Node (Black,c,z,d))
    | a,x,b ->
        Node (Black,a,x,b)

  let rbalance x2' x1' x3' = 
		match x1', x2', x3' with
    | a, x, Node (Red, Node (Red,b,y,c), z, d) ->
        Node (Red, Node (Black,a,x,b), y, Node (Black,c,z,d))
    | a, x, Node (Red, b, y, Node (Red,c,z,d)) ->
        Node (Red, Node (Black, a,x,b), y, Node (Black, c,z,d))
    | a,x,b ->
        Node (Black, a,x,b)

	let rec ins z s = 
		match s with 
      | Empty ->
          Node (Red, Empty, z, Empty)
      | Node (Red, a, y, b) ->
          if z < y then Node (Red, ins z a, y, b)
          else if z > y then Node (Red, a, y, ins z b)
          else s
      | Node (Black, a, y, b) ->
          if z < y then lbalance y (ins z a) b
          else if z > y then rbalance y a (ins z b)
          else s

  let add z s =
    match ins z s with
      | Node (Black, a, y, b) -> 
				Node (Black, a, y, b)
      | Node (Red, a, y, b) -> 
				Node (Black, a, y, b)
      (*| Empty -> assert false*)
	
	(*s Removal *)

  (* [unbalanced_left] repares invariant (2) when the black height of the
     left son exceeds (by 1) the black height of the right son *)

  let unbalanced_left lt = 
		match lt with
    | Node (Red, Node (Black, t1, x1, t2), x2, t3) ->
        lbalance x2 (Node (Red, t1, x1, t2)) t3, false
    | Node (Black, Node (Black, t1, x1, t2), x2, t3) ->
        lbalance x2 (Node (Red, t1, x1, t2)) t3, true
    | Node (Black, Node (Red, t1, x1, Node (Black, t2, x2, t3)), x3, t4) ->
        Node (Black, t1, x1, lbalance x3 (Node (Red, t2, x2, t3)) t4), false
    (*| _ ->
        assert false*)

  (* [unbalanced_right] repares invariant (2) when the black height of the
     right son exceeds (by 1) the black height of the left son *)

  let unbalanced_right rt =
		match rt with
    | Node (Red, t1, x1, Node (Black, t2, x2, t3)) ->
        rbalance x1 t1 (Node (Red, t2, x2, t3)), false
    | Node (Black, t1, x1, Node (Black, t2, x2, t3)) ->
        rbalance x1 t1 (Node (Red, t2, x2, t3)), true
    | Node (Black, t1, x1, Node (Red, Node (Black, t2, x2, t3), x3, t4)) ->
        Node (Black, rbalance x1 t1 (Node (Red, t2, x2, t3)), x3, t4), false
    (*| _ ->
        assert false*)

  (* [remove_min s = (s',m,b)] extracts the minimum [m] of [s], [s'] being the
     resulting set, and indicates with [b] whether the black height has
     decreased *)

  let rec remove_min tm = 
		match tm with
    (*| Empty ->
			assert false*)
    (* minimum is reached *)
    | Node (Black, Empty, x, Empty) ->
			Empty, x, true
    | Node (Black, Empty, x, Node (Red, l, y, n)) ->
			Node (Black, l, y, n), x, false
    (*| Black (Empty, _, Black _) ->
			assert false*)
    | Node (Red, Empty, x, n) ->
			n, x, false
    (* minimum is recursively extracted from [l] *)
    | Node (Black, l, x, n) ->
			let l',m,d = remove_min l in
			let t = Node (Black, l', x, n) in
			if d then
	  		let t,d' = unbalanced_right t in 
				t,m,d'
			else
	  		t, m, false
    | Node (Red, l, x, n) ->
			let l',m,d = remove_min l in
			let t = Node (Red, l', x, n) in
			if d then
	  		let t,d' = unbalanced_right t in 
				t,m,d'
			else
	  		t, m, false


  (* [remove_aux x s = (s',b)] removes [x] from [s] and indicates with [b]
     whether the black height has decreased *)
		
	let rec remove_aux x t = 
		match t with
      | Empty ->
	  		Empty, false
      | Node (Black, l, y, n) ->
			  if x < y then
			    let l',d = remove_aux x l in
			    let t = Node (Black, l', y, n) in
			    if d then unbalanced_right t 
					else t, false
			  else if x > y then
			    let n',d = remove_aux x n in
			    let t = Node (Black, l, y, n') in
			    if d then unbalanced_left t 
					else t, false
			  else (* x = y *)
			    (match n with
			       | Empty ->
				   			(match l with
									| Node (Red, l, x, n) -> 
										Node (Black, l, x, n), false
    							| Node (Black, a, b, c) -> 
										l, true
									| Empty -> 
										l, true
								)
			       | Node (Red, a, b, c) ->
				   			let n',m,d = remove_min n in
				   			let t = Node (Black, l, m, n') in
				   			if d then unbalanced_left t 
								else t, false
								
						 | Node (Black, a, b, c) ->		
								let n',m,d = remove_min n in
				   			let t = Node (Black, l, m, n') in
				   			if d then unbalanced_left t 
								else t, false
						)
  	 | Node (Red, l, y, n) ->
  			if x < y then
    			let l',d = remove_aux x l in
    			let t = Node (Red, l', y, n) in
    			if d then unbalanced_right t 
					else t, false
  			else if x > y then
    			let n',d = remove_aux x n in
    			let t = Node (Red, l, y, n') in
    			if d then unbalanced_left t 
					else t, false
  			else (* x = y *)
					(match n with
   					| Empty ->
   						l, false
   					| Node (Red, a, b, c) ->
   						let n',m,d = remove_min n in
   						let t = Node (Red, l, m, n') in
   						if d then unbalanced_left t 
							else t, false
						| Node (Black, a, b, c) ->
							let n',m,d = remove_min n in
   						let t = Node (Red, l, m, n') in
   						if d then unbalanced_left t 
							else t, false		
					)
			
	let remove x t =
    let t', b = remove_aux x t in t' 
	
	(*s The sorted list of elements *)

  let rec elements_aux accu t =
		match t with 
    | Empty ->
        accu
    | Node (Black, l, dv, n) ->
				elements_aux (dv :: elements_aux accu n) l
		| Node (Red, l, dv, n) ->
        elements_aux (dv :: elements_aux accu n) l

  let elements s =
    elements_aux [] s    
		
	let harness () = 
		let t = Node (Red, Empty, 0, Empty) in
		(add 0 t;	
		elements t;
		remove 0 t)