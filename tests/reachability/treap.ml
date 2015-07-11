	type 'a t =
    | Node of 'a * int * 'a t * 'a t
    | Leaf
        
  let empty = Leaf

  let is_empty t = 
		t = Leaf
      
  let balance t = 
		match t with
    | Node (elt, priority, left, right) ->
			(match left, right with
				| (Node (lelt, lpriority, lleft, lright)),
            (Node (relt, rpriority, rleft, rright)) -> 
					if lpriority > priority && lpriority >= rpriority then
          Node (lelt, lpriority,
            lleft,
            (Node (elt, priority, lright, right)))
        	else if rpriority > priority then
          Node (relt, rpriority,
            Node (elt, priority, left, rleft),
            rright)
       	 	else
          t
				| Node (lelt, lpriority, lleft, lright),
            (Leaf) -> 
					if lpriority > priority then
          Node (lelt, lpriority,
            lleft,
            Node (elt, priority, lright, right))
        	else
          t
				| (Leaf),
            Node (relt, rpriority, rleft, rright) ->
					if rpriority > priority then
          Node (relt, rpriority,
            Node (elt, priority, left, rleft),
            rright)
        	else
          t
				| Leaf, Leaf -> t
				)
    | Leaf -> t
        
  let rec add t new_elt new_priority =
    match t with
      | Node (elt, priority, left, right) ->
            if new_elt = elt then
              if new_priority <> priority then
                balance (Node (elt, new_priority, left, right))
              else
                t
            else if new_elt < elt then
              let left = add left new_elt new_priority in
                balance (Node (elt, priority, left, right))
            else
              let right = add right new_elt new_priority in
                balance (Node (elt, priority, left, right))
      | Leaf ->
          Node (new_elt, new_priority, Leaf, Leaf)

  let rec delete_root t =
    match t with
      | Node (elt, priority, left, right) ->
				(match left, right with
					| (Node (lelt, lpriority, lleft, lright)),
              (Node (relt, rpriority, rleft, rright)) ->
						if lpriority > rpriority then
            Node (lelt, lpriority,
              lleft,
              (delete_root (Node (elt, priority, lright, right))))
          	else
            Node (relt, rpriority,
              (delete_root (Node (elt, priority, left, rleft))),
              rright)
					| left, Leaf -> left
					| Leaf, right -> right
					)
			| Leaf ->
          Leaf 
            
  let rec delete t elt =
    match t with
      | Node (elt', priority, left, right) ->
            if elt = elt' then
              delete_root t
            else if elt < elt' then
              let left = delete left elt in
                Node (elt', priority, left, right)
            else
              let right = delete right elt in
                Node (elt', priority, left, right)
      | Leaf ->
          t
            
  let rec find t elt =
    match t with
      | Node (elt', priority, left, right) ->
            if elt = elt' then
              (true, priority)
            else if elt < elt' then
              find left elt
            else
              find right elt
      | Leaf ->
          (false, 0)