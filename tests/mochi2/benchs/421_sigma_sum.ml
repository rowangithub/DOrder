
let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let rec sigma f n =
  if n <= 0
  then 0
  else f n + sigma f (n-1)

let main n =
  assert (sigma sum n >= n)

let _ = main 3
let _ = main (-2)
