let add x y = x + y
let rec sum n =
  if n <= 0 then
    0
  else
    add n (sum (n-1))
let main n = assert (n <= sum n)

let _ = main 5