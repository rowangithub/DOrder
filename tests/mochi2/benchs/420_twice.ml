let twice f x = f (f x)
let f x = 2 * x

let main n =
  if n > 0
  then assert (twice f n > n) else ()

let _ = main 3
let _ = main (-2)
