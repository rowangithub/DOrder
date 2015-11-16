	type 'a heap = 
		| E 
		| T of int * 'a * 'a heap * 'a heap

  let rank heap = 
		match heap with
			|  E -> 0 
			|  T (rank,x,l,n) -> rank

  let makeT y a b =
    if rank a >= rank b then 
			T (rank b + 1, y, a, b)
    else 
			T (rank a + 1, y, b, a)

  let empty = E
  let is_empty h = h = E

  let rec merge h1 h2 = 
		match h1, h2 with
    | (h1, E) -> h1
    | (E, h2) -> h2
    | T (rank1, x, a1, b1), T (rank2, y, a2, b2) ->
        if x <= y then 
					makeT x a1 (merge b1 h2)
        else 
					makeT y a2 (merge h1 b2)

  let insert heap x = 
		merge (T (1, x, E, E)) heap
	
	
  let find_min heap =
		match heap with
			(*| E -> raise Empty*) 
			| T (rank, x, a, b) -> x
			
  let delete_min heap = 
		match heap with
			(*| E -> raise Empty *)
			| T (rank, x, a, b) -> (x, merge a b)
	let harness1 () = (delete_min E; insert E 0)