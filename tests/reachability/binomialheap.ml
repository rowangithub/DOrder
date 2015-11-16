  type 'a tree = 
		| Node of int * 'a * ('a tree) list
  (*type 'a heap = ('a tree) list*)

  (*let empty = []
  let is_empty ts = ts = []*)

  let rank t = match t with
		| (Node (rank, x, c)) -> rank
		
  let root t = match t with
		| (Node (rank, x, c)) -> x 

  let mylink t1 t2 =
		match (t1, t2) with
			| (Node (r1, x1, c1), Node (r2, x2, c2)) ->
				if x1 <= x2 then Node (r1 + 1, x1, t2 :: c1)
    		else Node (r1 + 1, x2, t1 :: c2)

  let rec ins_tree t mts = 
		match mts with
    | [] -> t::mts
    | t' :: ts' ->
        if rank t < rank t' then t :: mts
        else ins_tree (mylink t t') ts'
	
  let insert x ts = 
		let em = ([]: ('a tree) list) in 
		ins_tree (Node (0, x, em)) ts
		

  let rec merge ts1 ts2 = 
		match ts1, ts2 with
    | ts1, [] -> ts1
    | [], ts2 -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
        else ins_tree (mylink t1 t2) (merge ts1' ts2') 

  let rec remove_min_tree ts'' = 
		match ts'' with
    (*| [] -> raise Empty*)
    | t :: ts ->
			(match ts with
				| [] -> t, ts
				| t'::ts' -> 
					let t', ts' = remove_min_tree ts in
        	if (root t) <= (root t') then (t, ts)
        	else (t', t :: ts')
				)

	let reverse (l : ('a tree) list) =
		let rec aux (us : ('a tree) list) (ys : ('a tree) list) = 
			match us with
				| [] -> ys
				| x::zs -> aux zs (x::ys) in
		let em = ([]: ('a tree) list) in
    aux l em
		

  let find_delete_min ts' =
    let (t, ts2) = remove_min_tree ts' in
		match t with
			| Node (rank, x, ts1) -> 
    	(x, merge (reverse ts1) ts2 ) 
	
	let harness1 y = 
		(find_delete_min ([]: ('a tree) list);
		insert y ([]: ('a tree) list)
		)