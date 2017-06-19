let f g x =
  if x > 0 then
    g x
  else
    1
let decr x = x - 1
let main n = assert(f decr 3 > 0)
