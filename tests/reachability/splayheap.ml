  type 'a heap = 
		| E 
		| T of 'a heap * 'a * 'a heap

  (*let empty = E
  let is_empty h = h = E*)

  let rec partition pivot t = 
		match t with
    | E -> E, E
    | T (a, x, b) ->
        if x <= pivot then
          match b with
          | E -> t, E
          | T (b1, y, b2) ->
              if y <= pivot then
                let small, big = partition pivot b2 in
                T (T (a, x, b1), y, small), big
              else
                let small, big = partition pivot b1 in
                T (a, x, small), T (big, y, b2)
        else
          match a with
          | E -> E, t
          | T (a1, y, a2) ->
              if y <= pivot then
                let small, big = partition pivot a2 in
                T (a1, y, small), T (big, x, b)
              else
                let small, big = partition pivot a1 in
                small, T (big, y, T (a2, x, b))

  let insert x t = 
		let a, b = partition x t in 
		T (a, x, b)
	let harness () = insert 0 E 

  let rec merge s t = 
		match s, t with
    | E, t -> t
    | T (a, x, b), t ->
        let ta, tb = partition x t in
        T (merge ta a, x, merge tb b) 

  let rec find_min (phantom:bool) t : int = 
		match t with
    (*| E -> raise Empty*)
    | T (E, x, b) -> x
    | T (a, x, b) -> find_min phantom a

  let rec delete_min t = 
		match t with
    (*| E -> raise Empty*)
    | T (E, x, b) -> (b)
    | T (T (E, x, b), y, c) -> (T (b, y, c))
    | T (T (a, x, b), y, c) -> 
			let (t') = delete_min a in
			(T (t', x, T (b, y, c)))