	type 'a t = 
		| Empty 
		| Black of 'a t * 'a * 'a t 
		| Red of 'a t * 'a * 'a t

	type 'a enum =
    | End
    | More of 'a  * 'a t * 'a enum

  let rec enum s e =
    match s with
      | Empty -> e
      | Black(l, x, n) -> enum l (More(x, n, e))
			| Red(l, x, n) -> enum l (More(x, n, e))

  (* Invariants: (1) a red node has no red son, and (2) any path from the
     root to a leaf has the same number of black nodes *)

  (* Note the use of two constructors [Black] and [Red] to save space
     (resulting in longer code at a few places, e.g. in function [remove]).
     These red-black trees saves 20\% of space w.n.t Ocaml's AVL, which
     store the height into a fourth argument. *)

  (*s Implementation of the set operations; [empty], [is_empty], [mem]
      and [singleton] are trivial. *)

  let empty = Empty

  let is_empty t = 
		match t with
			| Empty -> true 
			| _ -> false

  let rec mem x t = 
		match t with
    | Empty -> false
    | Black (l, dv, n) ->
			if x = dv then true
			else if x < dv then
				mem x l
			else mem x n
		| Red (l, dv, n) ->
      if x = dv then true
			else if x < dv then
				mem x l
			else mem x n

  let singleton x = Black (Empty, x, Empty)

  (*s Insertion *)

  let lbalance x2 x1 x3 = 
		match x1, x2, x3 with
    | Red (Red (a,x,b), y, c), z, d ->
        Red (Black (a,x,b), y, Black (c,z,d))
    | Red (a, x, Red (b,y,c)), z, d ->
        Red (Black (a,x,b), y, Black (c,z,d))
    | a,x,b ->
        Black (a,x,b)

  let rbalance x2' x1' x3' = 
		match x1', x2', x3' with
    | a, x, Red (Red (b,y,c), z, d) ->
        Red (Black (a,x,b), y, Black (c,z,d))
    | a, x, Red (b, y, Red (c,z,d)) ->
        Red (Black (a,x,b), y, Black (c,z,d))
    | a,x,b ->
        Black (a,x,b)

	let rec ins z s = 
		match s with 
      | Empty ->
          Red (Empty, z, Empty)
      | Red (a, y, b) ->
          if z < y then Red (ins z a, y, b)
          else if z > y then Red (a, y, ins z b)
          else s
      | Black (a, y, b) ->
          if z < y then lbalance y (ins z a) b
          else if z > y then rbalance y a (ins z b)
          else s

  let add z s =
    match ins z s with
      | Black (a, y, b) -> Black (a, y, b)
      | Red (a, y, b) -> Black (a, y, b)
      (*| Empty -> assert false*)
	let harness () = add 0 Empty

  (*s Removal *)

  (* [unbalanced_left] repares invariant (2) when the black height of the
     left son exceeds (by 1) the black height of the right son *)

  let unbalanced_left t = 
		match t with
    | Red (Black (t1, x1, t2), x2, t3) ->
        lbalance x2 (Red (t1, x1, t2)) t3, false
    | Black (Black (t1, x1, t2), x2, t3) ->
        lbalance x2 (Red (t1, x1, t2)) t3, true
    | Black (Red (t1, x1, Black (t2, x2, t3)), x3, t4) ->
        Black (t1, x1, lbalance x3 (Red (t2, x2, t3)) t4), false
    (*| _ ->
        assert false*)

  (* [unbalanced_right] repares invariant (2) when the black height of the
     right son exceeds (by 1) the black height of the left son *)

  let unbalanced_right t =
		match t with
    | Red (t1, x1, Black (t2, x2, t3)) ->
        rbalance x1 t1 (Red (t2, x2, t3)), false
    | Black (t1, x1, Black (t2, x2, t3)) ->
        rbalance x1 t1 (Red (t2, x2, t3)), true
    | Black (t1, x1, Red (Black (t2, x2, t3), x3, t4)) ->
        Black (rbalance x1 t1 (Red (t2, x2, t3)), x3, t4), false
    (*| _ ->
        assert false*)

  (* [remove_min s = (s',m,b)] extracts the minimum [m] of [s], [s'] being the
     resulting set, and indicates with [b] whether the black height has
     decreased *)

  let rec remove_min t = 
		match t with
    (*| Empty ->
			assert false*)
    (* minimum is reached *)
    | Black (Empty, x, Empty) ->
			Empty, x, true
    | Black (Empty, x, Red (l, y, n)) ->
			Black (l, y, n), x, false
    (*| Black (Empty, _, Black _) ->
			assert false*)
    | Red (Empty, x, n) ->
			n, x, false
    (* minimum is recursively extracted from [l] *)
    | Black (l, x, n) ->
			let l',m,d = remove_min l in
			let t = Black (l', x, n) in
			if d then
	  		let t,d' = unbalanced_right t in t,m,d'
			else
	  		t, m, false
    | Red (l, x, n) ->
			let l',m,d = remove_min l in
			let t = Red (l', x, n) in
			if d then
	  		let t,d' = unbalanced_right t in t,m,d'
			else
	  		t, m, false


  (* [remove_aux x s = (s',b)] removes [x] from [s] and indicates with [b]
     whether the black height has decreased *)
		
	let rec remove_aux x t = 
		match t with
      | Empty ->
	  		Empty, false
      | Black (l, y, n) ->
			  if x < y then
			    let l',d = remove_aux x l in
			    let t = Black (l', y, n) in
			    if d then unbalanced_right t else t, false
			  else if x > y then
			    let n',d = remove_aux x n in
			    let t = Black (l, y, n') in
			    if d then unbalanced_left t else t, false
			  else (* x = y *)
			    (match n with
			       | Empty ->
				   			(match l with
									| Red (l, x, n) -> Black (l, x, n), false
    							| Black (a, b, c) -> l, true
									| Empty -> l, true
								)
			       | Red (a, b, c) ->
				   			let n',m,d = remove_min n in
				   			let t = Black (l, m, n') in
				   			if d then unbalanced_left t else t, false
								
						 | Black (a, b, c) ->		
								let n',m,d = remove_min n in
				   			let t = Black (l, m, n') in
				   			if d then unbalanced_left t else t, false
						)
  	 | Red (l, y, n) ->
  			if x < y then
    			let l',d = remove_aux x l in
    			let t = Red (l', y, n) in
    			if d then unbalanced_right t else t, false
  			else if x > y then
    			let n',d = remove_aux x n in
    			let t = Red (l, y, n') in
    			if d then unbalanced_left t else t, false
  			else (* x = y *)
					(match n with
   					| Empty ->
   						l, false
   					| Red (a, b, c) ->
   						let n',m,d = remove_min n in
   						let t = Red (l, m, n') in
   						if d then unbalanced_left t else t, false
						| Black (a, b, c) ->
							let n',m,d = remove_min n in
   						let t = Red (l, m, n') in
   						if d then unbalanced_left t else t, false		
					)
			
	let remove x t =
    let t', b = remove_aux x t in t'
	let harness1 () = remove 0 Empty 
	
	(*s The sorted list of elements *)

  let rec elements_aux accu t =
		match t with 
    | Empty ->
        accu
    | Black (l, dv, n) ->
				elements_aux (dv :: elements_aux accu n) l
		| Red (l, dv, n) ->
        elements_aux (dv :: elements_aux accu n) l

  let elements s =
    elements_aux [] s    
	let harness2 () = elements Empty
	
	(*let rec union_aux e1' e2' accu =
	  match e1', e2' with
	    | End, End ->
	        accu
	    | End, More(x, n, e) ->
					union_aux End (enum n e) (add x accu)
	    | More(x, n, e), End ->
	        union_aux End (enum n e) (add x accu)
	    | (More(x1, r1, e1)), (More(x2, r2, e2)) ->
	      if x1 < x2 then union_aux (enum r1 e1) e2' (add x1 accu)
	      else if x1 > x2 then union_aux e1' (enum r2 e2) (add x2 accu)
	      else union_aux (enum r1 e1) (enum r2 e2) (add x1 accu)
	let union s1 s2 =
		union_aux (enum s1 End) (enum s2 End) Empty


	let rec inter_aux e1' e2' accu =
	  match e1', e2' with
	    | End, e2' -> accu
	    | e1', End -> accu
	    | (More(x1, r1, e1)), (More(x2, r2, e2)) ->
	        if x1 < x2 then inter_aux (enum r1 e1) e2' accu
	        else if x1 > x2 then inter_aux e1' (enum r2 e2) accu
	        else inter_aux (enum r1 e1) (enum r2 e2) (add x1 accu)
  let inter s1 s2 =
    inter_aux (enum s1 End) (enum s2 End) Empty


	let rec diff_aux e1' e2' accu =
      match e1', e2' with
        | End, e2' ->
            accu
        | More(x, n, e), End ->
            diff_aux (enum n e) End (add x accu)
        | (More(x1, r1, e1)), (More(x2, r2, e2)) ->
            if x1 < x2 then diff_aux (enum r1 e1) e2' (add x1 accu)
            else if x1 > x2 then diff_aux e1' (enum r2 e2) accu
            else diff_aux (enum r1 e1) (enum r2 e2) accu
  let diff s1 s2 =
    diff_aux (enum s1 End) (enum s2 End) Empty *)