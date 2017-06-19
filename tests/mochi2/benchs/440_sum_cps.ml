let rec cps_sum n k : unit =
  if n <= 0 then
    k 0
  else
    let f x = k (x + n) in
    cps_sum (n-1) f
let main n =
  let f x = assert (x >= n) in
  cps_sum n f
