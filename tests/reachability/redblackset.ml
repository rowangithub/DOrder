  type color = R | B
  type 'a tree = 
		| E 
		| T of color * 'a tree * 'a * 'a tree

  let empty = E

  let rec member t m = 
		match t with
    | E -> false
    | T (c, a, y, b) ->
        if m < y then member a m
        else if y < m then member b m
        else true

  let balance tr = 
		match tr with
    | T (B, T (R, T (R, a, x, b), y, c), z, d) ->
				T (R, T (B, a, x, b), y, T (B, c, z, d))
    | T (B, T (R, a, x, T (R, b, y, c)), z, d) ->
				T (R, T (B, a, x, b), y, T (B, c, z, d))
    | T (B, a, x, T (R, T (R, b, y, c), z, d)) ->
				T (R, T (B, a, x, b), y, T (B, c, z, d))
    | T (B, a, x, T (R, b, y, T (R, c, z, d))) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | T (a, b, c, d) -> T (a, b, c, d)

	let rec ins t x = 
		match t with
  	| E -> T (R, E, x, E)
  	| T (color, a, y, b) ->
      if x < y then balance (T (color, ins a x, y, b))
      else if y < x then balance (T (color, a, y, ins b x))
      else t

  let insert s x =
    match ins s x with  (* guaranteed to be non-empty *)
    | T (color, a, y, b) -> T (B, a, y, b)
    (*| _ -> impossible_pat "insert"*) 
	let harness () = insert E 0