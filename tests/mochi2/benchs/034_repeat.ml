let succ x = x + 1

let rec repeat (f:int->int) n s =
  if n = 0 then
    s
  else
    f (repeat f (n-1) s)
		
let main n = assert (repeat succ n 0 >= n)