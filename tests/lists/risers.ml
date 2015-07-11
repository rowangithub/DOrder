let rec make_list m =
  if m <= 0
  then []
  else (m) :: make_list (m-1)

let risersElse (x:int) xs = match xs with
    [] -> assert false
  | s::ss -> [x]::s::ss

let risersThen (x:int) xs = match xs with
    [] -> assert false
  | s::ss -> (x::s)::ss

let rec risers (xs:int list) = match xs with
    [] -> []
  | [x] -> [[x]]
  | x::y::etc ->
       if (Random.bool ()) then risersThen x (risers (y::etc))
			 else risersElse x (risers (y::etc))
				
let main m = risers (make_list m)

let _ = main 0
let _ = main 2