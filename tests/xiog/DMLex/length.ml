(* sort nat = {a:int | a >= 0} *)
fun ('a)
  length [] = 0
| length (x :: xs) = 1 + length (xs)
withtype {n:nat} <n> => 'a list(n) -> int(n)
