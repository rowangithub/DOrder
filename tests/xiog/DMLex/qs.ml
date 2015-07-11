fun par (x, rest, l, g) =
  case rest of
    [] => (l, g)
  | y :: rest =>
      if x <= y then par (x, rest, l, y :: g)
      else par (x, rest, y :: l, g)
withtype
  {n:nat,p:nat,q:nat} <n> =>
    int * int list(n) * int list(p) * int list(q) ->
    [p':nat,q':nat | p'+q'=n+p+q] (int list(p') * int list(q'))

fun qs [] = []
  | qs (x :: rest) =
    let
       val (l, g) = par (x, rest, [], [])
    in
       qs (l) @ qs (x :: g)
    end
withtype {n:nat} <n> => int list(n) -> int list(n)


