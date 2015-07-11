fun('a,'b)
  zip [] [] = []
| zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
withtype {n:nat} <n> => 'a list(n) -> 'b list(n) -> ('a * 'b) list(n)