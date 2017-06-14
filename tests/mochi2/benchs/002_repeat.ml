let rec succ x = x + 1
and repeat f n s =
  if n = 0 then
    s
  else
    f (repeat f (n - 1) s)
and main n = assert (repeat succ n 0 >= n)

let _ = main 3
let _ = main (-2)
