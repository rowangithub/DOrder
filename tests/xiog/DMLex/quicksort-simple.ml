fun('a)
  par cmp (x, l, r, xs) =
    case xs of
     [] => (l, r)
    | x' :: xs' => if cmp(x', x) then par cmp (x, x' :: l, r, xs')
                   else par cmp (x, l, x' :: r, xs')
withtype ('a * 'a -> bool) ->
         {p:nat,q:nat,r:nat} <r> =>
         'a * 'a list(p) * 'a list(q) * 'a list(r) ->
         [p':nat,q':nat | p'+q'=p+q+r] ('a list(p') * 'a list(q'))

fun('a)
  qs cmp xs =
    case xs of
      [] => []
    | x :: xs' =>
      let
          val (l, r) = par cmp (x, [], [], xs')
      in
          (qs cmp l) @ (x :: (qs cmp r))
      end
withtype ('a * 'a -> bool) -> {n:nat} <n> => 'a list(n) -> 'a list(n)
