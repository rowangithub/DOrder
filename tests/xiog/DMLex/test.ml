fun id x = x
withtype {a:int} int(a) -> int

val y = id (id 0 + id 0)

(*
val x = #"c"

val x' = 1 :: [2,3]

val id = fn x => x

sort pos = {a:int | a > 0}

val (x, y) = (~1, 1)

datatype 'a list with nat =
  nil(0) | {n:nat} cons(n+1) of 'a * 'a list(n)

fun('a)
    length nil = 0
  | length (cons (_, xs)) = length (xs) + 1
withtype {n:nat} 'a list(n) -> int(n)

and('a)
  length xs =
    case xs of
      nil => 0
    | cons (_, xs) => 1 + length (xs)
withtype {n:nat} <n> => 'a list(n) -> int(n)

fun length xs =
  let
      fun('a) len (nil, n) = n
        | len (cons (x, xs), n) = len (xs, n+1)
      withtype {i:nat}{j:nat} <i> => 'a list(i) * int(j) -> int(i+j)
  in
      len (xs, 0)
  end
withtype {n:nat} <> => int list(n) -> int(n)

fun('a)
  revApp xs = fn
    nil => xs
  | cons (y, ys) => revApp (cons (y, xs)) ys
withtype {m:nat} 'a list(m) ->
         {n:nat} <n> => 'a list(n) -> 'a list(m+n)
*)