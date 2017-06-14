let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 0 then
    -1
  else if x <= 2 then
    3 - x
  else
    x
let main n =
  assert(f 3 >= 0)

let _ = main 1
