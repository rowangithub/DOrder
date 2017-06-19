let rec f x =
  if x < -1 then
    f (-2)
  else if x <= 1 then
    2 * x - 1
  else
    x
let main n =
  assert(f 3 >= 0)
