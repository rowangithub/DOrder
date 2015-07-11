let produce _ =
  (5, 20)

let _ = match produce () with (a, b) -> assert(a < b)
