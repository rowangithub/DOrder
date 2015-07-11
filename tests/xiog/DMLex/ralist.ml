datatype 'a ralist with nat =
  Nil(0)
| One(1) of 'a
| {n:pos} Even (n+n) of 'a ralist(n) * 'a ralist(n)
| {n:pos} Odd (n+n+1) of 'a ralist(n+1) * 'a ralist(n)

(*
 suppose l1 = x1, ..., xn; l2 = y1, ..., yn;
 Even (l1, l2) = x1, y1, ..., xn, yn

 suppose l1 = x1, ..., xn, x; l2 = y1, ..., yn;
 Odd (l1, l2) = x1, y1, ..., xn, yn, x

 1,2,3,4,5

 Odd(Odd (Even(One(1), One(5)); One(3)); Even(One(2), One(4))
*)

fun ('a)
  cons (x, xs) =
    case xs of
      Nil => One (x)
    | One (y) => Even (One(x), One(y))
    | Even (xs', xs'') => Odd (cons (x, xs''), xs')
    | Odd (xs', xs'') => Even (cons (x, xs''), xs')
withtype {n:nat} <n> => 'a * 'a ralist(n) -> 'a ralist(n+1)

fun ('a)
  uncons xs =
    case xs of
      One (x) => (x, Nil)
    | Even (xs', xs'') =>
      let
          val (x, xs') = uncons xs'
      in
          (x, case xs' of
                Nil => xs''
              | One _ => Odd (xs'', xs')
              | Even _ => Odd (xs'', xs')
              | Odd _ => Odd (xs'', xs'))
      end
    | Odd (xs', xs'') =>
      let
          val (x, xs') = uncons xs'
      in
          (x, Even (xs'', xs'))
      end
withtype {n:pos} <n> => 'a ralist(n) -> 'a * 'a ralist(n-1)

fun ('a)
  revApp (xs, ys) =
    case xs of
      Nil => ys
    | One x => cons (x, ys)
    | Even _ =>
      let
         val (x, xs) = uncons xs
      in
         revApp (xs, cons (x, ys))
      end
    | Odd _ =>
      let
         val (x, xs) = uncons xs
      in
         revApp (xs, cons (x, ys))
      end
withtype {m:nat,n:nat} <m> =>
         'a ralist(m) * 'a ralist(n) -> 'a ralist(m+n)

fun ('a) reverse xs = revApp (xs, Nil)
withtype {n:nat} <> => 'a ralist(n) -> 'a ralist(n)

fun ('a)
  reverse Nil = Nil
| reverse (One x) = One x
| reverse (Even (xs, ys)) = Even (reverse ys, reverse xs)
| reverse (Odd (xs, ys)) = Odd (reverse xs, reverse ys)
withtype {n:nat} <n> => 'a ralist(n) -> 'a ralist(n)
