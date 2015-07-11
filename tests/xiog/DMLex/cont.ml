fun f [] k = k []
  | f (x :: xs) k =
    if x <= 0 then k (xs) else f xs (fn xs' => x :: f xs' k) ;

fun f x y k = (* k is a continuation *)
  if x = y then k x else f (x-1) y (fn x' => x * (f x' y k))
withtype {m:nat} <m> => int(m) ->
         {n:nat | n <= m} int(n) ->
         ({m':nat | n <= m' <= m} int(m') -> int) -> int






