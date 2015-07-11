fun ('a) append xs ys =
  case xs of [] => ys | x :: xs => x :: append xs ys
withtype {m:nat,n:nat} <m> =>
         'a list(m) -> 'a list(n) -> 'a list(m+n)

fun ('a)
  reverse [] = []
| reverse (x :: xs) = append (reverse (xs)) [x]
withtype {n:nat} <n> => 'a list(n) -> 'a list(n)
