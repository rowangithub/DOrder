datatype 'a ralist with nat =
    Nil(0)
  | One(1) of 'a
  | {n:nat | n > 0} Even(n+n) of ('a * 'a) ralist(n)
  | {n:nat | n > 0} Odd(n+n+1) of 'a * ('a * 'a) ralist(n)

fun('a)
  cons (x, Nil) = One x
| cons (x, One y) = Even(One (x, y))
| cons (x, Even(l)) = Odd(x, l)
| cons (x, Odd(y, l)) = Even(cons ((x, y), l))
withtype {n:nat} <n> => 'a * 'a ralist(n) -> 'a ralist(n+1)

fun('a)
  uncons (One x) = (x, Nil)
| uncons (Even l) =
  let
      val ((x, y), l) = uncons (l)
  in
      case l of
	  Nil => (x, One y)
	| One _ => (x, Odd (y, l))
	| Even _ => (x, Odd (y, l))
	| Odd _ => (x, Odd (y, l))
  end
| uncons (Odd (x, l)) = (x, Even l)
withtype {n:nat | n > 0} <n> => 'a ralist(n) -> 'a * 'a ralist(n-1)

fun('a) head l = let val (x, _) = uncons l in x end
withtype {n:nat | n > 0} <> => 'a ralist(n) -> 'a

fun('a) tail l = let val (_, l) = uncons l in l end
withtype {n:nat | n > 0} <> => 'a ralist(n) -> 'a ralist(n-1)

fun('a) lookup_safe (i, l) =
  case l of
    One (x) => x
  | Odd (x, l) => if i = 0 then x else lookup_safe (i-1, Even l)
  | Even l =>
	let
	    val (x, y) = lookup_safe (i / 2, l)
	in
	    if i % 2 = 0 then x else y
	end
withtype {i:nat, n:nat | i < n} <n> =>  int(i) * 'a ralist(n) -> 'a

fun('a) fupdate_safe (f, i, l) =
  case l of
    One (x) => One (f x)
  | Odd (x, l) =>
	if i = 0 then Odd (f x, l)
	else cons (x, fupdate_safe (f, i-1, Even l))
  | Even l =>
	let
	    fun f' (x, y) = if i % 2 = 0 then (f x, y) else (x, f y)
            withtype 'a * 'a -> 'a * 'a
	in
	    Even (fupdate_safe (f', i / 2, l))
	end
withtype {i:nat, n:nat | i < n} <n> =>
         ('a -> 'a) * int(i) * 'a ralist(n) -> 'a ralist(n)

fun('a) update_safe (i, l) = fupdate_safe ((fn x => x: 'a -> 'a), i, l)
withtype {i:nat, n:nat | i < n} <> => int(i) * 'a ralist(n) -> 'a ralist(n)
