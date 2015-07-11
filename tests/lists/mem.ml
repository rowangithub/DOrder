let rec mem (x:int) (xs:int list) =
  match xs with
      [] -> 0
    | x'::xs -> 
			if x = x' then 1
			else if (mem x xs > 0) then 1
			else 0

let rec make_list n (x:int) =
  if n < 0
  then []
  else x :: make_list (n-1) x

let is_nil (xs:int list) =
  match xs with
      [] -> 1
    | _ -> 0

let main n m =
  let xs = make_list n m in
	match xs with
		| [] -> ()
		| x::xs' ->
		assert (mem m xs > 0)
		

let _ = main 1 0