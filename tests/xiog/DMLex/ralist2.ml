datatype 'a ralist with nat =
    Nil(0)
  | One(1) of 'a
  | {n:nat | n > 0} Even(n+n) of 'a ralist(n) * 'a ralist(n)
  | {n:nat | n > 0} Odd(n+n+1) of 'a ralist(n+1) * 'a ralist(n)

fun('a)
  cons (x, Nil) = One x
| cons (x, One y) = Even(One(x), One(y))
| cons (x, Even(l1, l2)) = Odd(cons (x, l2), l1)
| cons (x, Odd(l1, l2)) = Even(cons (x, l2), l1)
withtype {n:nat} <n> => 'a * 'a ralist(n) -> 'a ralist(n+1)

fun('a)
  uncons (One x) = (x, Nil)
| uncons (Even(l1, l2)) =
  let
      val (x, l1) = uncons l1
  in
      case l1 of
	  Nil => (x, l2)
	| One _ => (x, Odd(l2, l1))
	| Even _ => (x, Odd(l2, l1))
	| Odd _ => (x, Odd(l2, l1))
  end
| uncons (Odd(l1, l2)) =
  let val (x, l1) = uncons l1 in (x, Even(l2, l1)) end
withtype {n:nat | n > 0} <n> => 'a ralist(n) -> 'a * 'a ralist(n-1)

fun('a)
  length (Nil) = 0
| length (One _) = 1
| length (Even (l1, _)) = 2 * (length l1)
| length (Odd (_, l2)) = 2 * (length l2) + 1
withtype {n:nat} <n> => 'a ralist(n) -> int(n)

fun('a)
  lookup_safe (i, One x) = x
| lookup_safe (i, Even(l1, l2)) =
    if i % 2 = 0 then lookup_safe (i / 2, l1)
    else lookup_safe (i / 2, l2)
| lookup_safe (i, Odd(l1, l2)) =
    if i % 2 = 0 then lookup_safe (i / 2, l1)
    else lookup_safe (i / 2, l2)
withtype {i:nat, n:nat | i < n} <n> => int(i) * 'a ralist(n) -> 'a

fun('a)
  update_safe (i, x, One y) = One x
| update_safe (i, x, Even(l1, l2)) =
  if i % 2 = 0 then Even(update_safe (i / 2, x, l1), l2)
  else Even(l1, update_safe (i / 2, x, l2))
| update_safe (i, x, Odd(l1, l2)) =
  if i % 2 = 0 then Odd(update_safe (i / 2, x, l1), l2)
  else Odd(l1, update_safe (i / 2, x, l2))
withtype {i:nat, n:nat | i < n} <n> =>
         int(i) * 'a * 'a ralist(n) -> 'a ralist(n)

exception Not_found

fun('a) lookup (i, l) =
  if i < 0 then raise Not_found
  else if i >= length l then raise Not_found
       else lookup_safe (i, l)
withtype <> => int * 'a ralist -> 'a
