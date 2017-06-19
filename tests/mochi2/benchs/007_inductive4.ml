let rec f g x =
  if x < -3 then
    f g (-4)
  else if x <= 1 then
    g x
  else
    f (f g) (x - 2)
let incr x = x + 1
let main n =
  assert(f incr 3 >= -3)
