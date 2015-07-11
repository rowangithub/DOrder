let rec length (xs:int list) =
  match xs with
      [] -> 0
    | x::xs' -> 1 + length xs'

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n =
  let xs = make_list n in
  assert (length xs = n)
		
let _ = main 1