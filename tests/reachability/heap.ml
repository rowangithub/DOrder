  type 'a t =
    | Empty
    | Same of 'a t * 'a * 'a t (* same number of elements on both sides *)
    | Diff of 'a t * 'a * 'a t (* left has [n+1] nodes and right has [n] *)

  (*let empty = Empty*)

  let rec add heap x =
		match heap with
    | Empty ->
        Same (Empty, x, Empty)
    (* insertion to the left *)
    | Same (l, y, n) ->
        if x > y then Diff (add l y, x, n) else Diff (add l x, y, n)
    (* insertion to the right *)
    | Diff (l, y, n) ->
        if x > y then Same (l, x, add n y) else Same (l, y, add n x)

  let maximum (phantom:bool) mheap : int =
		match mheap with
    (*| Empty -> raise EmptyHeap*)
    | Same (l, x, n) -> x
		| Diff (l, x, n) -> x

  (* extracts one element on the bottom level of the tree, while
     maintaining the representation invariant *)
  let rec extract_last heap = 
		match heap with
    (*| Empty -> raise EmptyHeap*)
    | Same (Empty, x, Empty) -> x, Empty
    | Same (l, x, n) -> let y,n' = extract_last n in y, Diff (l, x, n')
    | Diff (l, x, n) -> let y,l' = extract_last l in y, Same (l', x, n)

  (* removes the topmost element of the tree and inserts a new element [x] *)
  let rec descent x heap = 
		match heap with
    (*| Empty ->
        assert false*)
    | Same (l, x', n) ->
			(x', match l, n with
				| Empty, Empty -> 
					(Same (Empty, x, Empty))
				| l, n ->	
	        let ml = maximum true l in
	        let mr = maximum true n in
	        if x > ml && x > mr then
	          (Same (l, x, n))
	        else
	          if ml > mr then
							let (value, l) = descent x l in
	            (Same (l, ml, n))
	          else
							let (value, n) = descent x n in
	            (Same (l, mr, n)) )
    | Diff (l, x', n) ->
			(x', match l, n with
				| Same (Empty, z, Empty), Empty -> 
					if x > z then (Diff (Same (Empty, z, Empty), x, Empty))
        	else (Diff (Same (Empty, x, Empty), z, Empty))
				| l, n ->
	        let ml = maximum true l in
	        let mr = maximum true n in
	        if x > ml && x > mr then
	          (Diff (l, x, n))
	        else
	          if ml > mr then
							let (value, l) = descent x l in 
	            (Diff (l, ml, n))
	          else
							let (value, n) = descent x n in
	            (Diff (l, mr, n)) )

  let remove h = 
		match h with
    (*| Empty -> raise EmptyHeap*)
    | Same (Empty, x, Empty) -> (Empty)
    | Same (a,b,c) -> 
			let y,h' = extract_last h in 
			let value, result = descent y h' in
			result
		| Diff (a,b,c) ->
			let y,h' = extract_last h in 
			let value, result = descent y h' in
			result
	let harness () = remove Empty