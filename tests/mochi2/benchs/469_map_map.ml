let rec map x =
 if x=0 then x else 1 + map (x-1)

let main n =
  assert(map (map n) = n)

let _ = main 2
let _ = main (-1)
