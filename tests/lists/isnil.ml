let is_nil (xs:int list) =
  match xs with
      [] -> 1
    | _ -> 0

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let main n =
  let xs = make_list n in
    if n > 0
    then assert ((is_nil xs) != 1)
    else ()
		
let _ = main 3