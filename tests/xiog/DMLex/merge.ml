fun
  merge ([], ys) = ys
| merge (xs, []) = xs
| merge (xs as x :: xs', ys as y :: ys') =
  if x < y then x :: merge (xs', ys)
  else y :: merge (xs, ys')
withtype {m:nat,n:nat} 
            int list (m) * int list(n) -> int list(m+n)


fun ('a)
  alternate ([], ys) = ys
| alternate (x :: xs, ys) = x :: alternate (ys, xs)
withtype {m:nat,n:nat} <m+n> =>
            'a list(m) * 'a list(n) -> 'a list(m+n)

