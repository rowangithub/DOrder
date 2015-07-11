fun('a)
  qs cmp xs =
    case xs of
      [] => []
    | x :: xs' => par cmp (x, [], [], xs')
withtype ('a * 'a -> bool) -> {n:nat} <n,0> => 'a list(n) -> 'a list(n)

and('a)
  par cmp (x, l, r, xs) =
    case xs of
      [] => qs cmp l @ (x :: qs cmp r)
    | x' :: xs' => if cmp(x', x) then par cmp (x, x' :: l, r, xs')
                 else par cmp (x, l, x' :: r, xs')
withtype ('a * 'a -> bool) ->
         {p:nat,q:nat,r:nat} <p+q+r,r+1> =>
         'a * 'a list(p) * 'a list(q) * 'a list(r) -> 'a list(p+q+r+1)
