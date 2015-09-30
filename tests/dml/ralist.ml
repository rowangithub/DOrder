(* adapted from an example by Xi
 * http://www.cs.bu.edu/~hwxi/DML/DML/examples/ralist.dml
 *)

type 'a ralist = 
	| Nil
	| One of 'a
	| Even of 'a ralist * 'a ralist
	| Odd of 'a ralist * 'a ralist

let rec size xs = match xs with
	| Nil -> 0 | One x -> 1
	| Even (t1, t2) -> size t1 + size t2
	| Odd (t1, t2) -> size t1 + size t2

let rec treebal t = match t with
	| Nil -> 1
	| One x -> 1
	| Even (t1, t2) ->
		if (size t1 >= size t2 && size t1 <= size t2 + 1
				&& treebal t1 = 1 && treebal t2 = 1) then 1
		else 0
	| Odd (t1, t2) ->
		if (size t1 >= size t2 && size t1 <= size t2 + 1
				&& treebal t1 = 1 && treebal t2 = 1) then 1
		else 0

let rec cons x xs = 
	match xs with
		| Nil -> One x
    | One y -> Even (One(x), One(y))
    | Even (xs', xs'') -> Odd (cons x xs'', xs')
    | Odd (xs', xs'') -> Even (cons x xs'', xs')
(*withtype {n:nat} <n> => 'a * 'a ralist(n) -> 'a ralist(n+1)*)

let rec uncons xs =
	match xs with
    | One x -> (x, Nil)
    | Even (xs', xs'') ->
      let (x, xs') = uncons xs' in
          (x, match xs' with
                Nil -> xs''
              | One y -> Odd (xs'', xs')
              | Even (ys, ys') -> Odd (xs'', xs')
              | Odd (ys, ys') -> Odd (xs'', xs'))
    | Odd (xs', xs'') ->
      let (x, xs') = uncons xs' in
      (x, Even (xs'', xs'))
(*withtype {n:pos} <n> => 'a ralist(n) -> 'a * 'a ralist(n-1)*)

let rec makelist n =
  if n = 0 then Nil 
	else
    let l = (makelist (n-1)) in
    cons n l

let rec revApp xs ys =
	match xs with
		| Nil -> ys
    | One x -> cons x ys
    | Even (a, b) ->
      let (x, xs) = uncons xs in
      revApp xs (cons x ys)
    | Odd (a, b) ->
      let (x, xs) = uncons xs in
      revApp xs (cons x ys)
(*withtype {m:nat,n:nat} <m> =>
         'a ralist(m) * 'a ralist(n) -> 'a ralist(m+n)*)

let reverse xs = revApp xs Nil
(*withtype {n:nat} <> => 'a ralist(n) -> 'a ralist(n)*)

let rec reverse2 xs = 
	match xs with
		| Nil -> Nil
		| (One x) -> One x
		| (Even (xs, ys)) -> Even (reverse2 ys, reverse2 xs)
		| (Odd (xs, ys)) -> Odd (reverse2 xs, reverse2 ys)
(*withtype {n:nat} <n> => 'a ralist(n) -> 'a ralist(n)*)

let rec lookup i l =
  match l with
		| Nil -> assert false
		| One x -> (assert (i = 0); x)
  	| Odd (l1, l2) -> 
			if i mod 2 = 0 then 
				lookup (i/2) l1
			else 
				lookup (i/2) l2
  	| Even (l1, l2) ->
			if i mod 2 = 0 then 
				lookup (i/2) l1
			else 
				lookup (i/2) l2

let main n = 
	if (n > 0) then
		let xs = makelist n in
		let xs' = reverse xs in
		let xs' = reverse2 xs in
		let x = lookup (n/2) xs in
		assert (size xs' = size xs)
	else ()
let _ = main 10