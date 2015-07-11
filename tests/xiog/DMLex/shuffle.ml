(*
fun ('a)
  shuffle nil = nil
| shuffle xs =
  case reverse xs of x :: xs => x :: shuffle (xs)
withtype {n:nat} 'a list(n) -> 'a list(n)
*)

fun ('a)
  shuffle nil = nil
| shuffle l =
    case (reverse l) of
      x :: xs => cons (x, shuffle xs)
withtype {n:nat} <n> => 'a list(n) -> 'a list(n)
