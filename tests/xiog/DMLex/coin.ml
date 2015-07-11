val dominations = [1,5,10,25]

fun f n v =
  case ds of
    [] => if n = 0 then v = 0 else false
  | d :: ds =>
      if v < d then false
      else if n = 0 then if v = d then true else f ds n v
      else if f dominations (n-1) (v-d) then true else f ds n v

fun changes n v =
  if n = 0 then v = 0
  else f dominations (n-1) v
