type 'a ralist = 
	| Nil
	| One of 'a
	| Even of 'a ralist * 'a ralist
	| Odd of 'a ralist * 'a ralist

let rec size xs = match xs with
	| Nil -> 0
	| One x -> 1
	| Even (t1, t2) -> size t1 + size t2
	| Odd (t1, t2) -> size t1 + size t2

let rec cons x xs = 
	match xs with
		| Nil -> One x
    | One y -> 
			Even (One(x), One(y))
    | Even (xs', xs'') -> 
			Odd (cons x xs'', xs')
    | Odd (xs', xs'') -> 
			Even (cons x xs'', xs')

let rec uncons xs =
	match xs with
    | One x -> (x, Nil)
    | Even (xs', xs'') ->
      let (x, xs') = uncons xs' in
          (x, match xs' with
              | Nil -> xs''
              | One y -> Odd (xs'', xs')
              | Even (ys, ys') -> Odd (xs'', xs')
              | Odd (ys, ys') -> Odd (xs'', xs'))
    | Odd (xs', xs'') ->
      let (x, xs') = uncons xs' in
      (x, Even (xs'', xs'))


let rec reverse xs = 
	match xs with
		| Nil -> Nil
		| (One x) -> One x
		| (Even (xs, ys)) -> 
			Even (reverse ys, reverse xs)
		| (Odd (xs, ys)) -> 
			Odd (reverse xs, reverse ys) 
			

let rec update i x xs =
	match xs with
		| One y -> One x
		| Even (l1, l2) ->
			if i mod 2 = 0 then 
				Even(update (i/2) x l1, l2)
			else 
				Even(l1, update (i/2) x l2)
		| Odd (l1, l2) ->
			if i mod 2 = 0 then 
				Odd(update (i/2) x l1, l2)
			else 
				Odd(l1, update (i/2) x l2)
			
let rec lookup i l : int =
  match l with
		| One x -> x
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