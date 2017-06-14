let rec map x =
  if x = 0
  then 0
  else 1 + map (x-1)

let main n =
  assert (map n = n)

let _ = main 3
let _ = main (-2)
