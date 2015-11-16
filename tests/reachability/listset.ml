let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)


let rec merge_aux acc (ul1, ul2) =
	match ul1, ul2 with 
  | [], ul2 ->
      rev_append ul2 acc
  | ul1, [] ->
      rev_append ul1 acc
  | (x1 :: r1), (x2 :: r2) ->
      if x1 < x2 then merge_aux (x1 :: acc) (r1, ul2)
      else if x1 > x2 then merge_aux (x2 :: acc) (ul1, r2)
      else merge_aux (x1 :: acc) (r1, r2)

let union ul1 ul2 =
    merge_aux [] (ul1, ul2)
		
let rec inter_aux acc (il1, il2) =
	match il1, il2 with 
  | [], il2 -> acc 
	| il1, [] -> acc
  | (x1 :: r1), (x2 :: r2) ->
	  if x1 = x2 then 
			inter_aux (x1 :: acc) (r1, r2)
	  else if x1 < x2 then 
			inter_aux acc (r1, il2)
	  else (* x1 > x2 *) 
			inter_aux acc (il1, r2)
			
let inter il1 il2 =
    inter_aux [] (il1, il2)

let rec diff_aux acc (dl1, dl2) = 
	match dl1, dl2 with
  | [], dl2 ->
      acc
  | dl1, [] ->
     	rev_append dl1 acc
  | (x1 :: r1), (x2 :: r2) ->
      if x1 = x2 then diff_aux acc (r1, r2)
      else if x1 < x2 then diff_aux (x1 :: acc) (r1, dl2)
      else (* x1 > x2 *) diff_aux acc (dl1, r2)		
						
let diff dl1 dl2 =
    diff_aux [] (dl1, dl2)	

let harness () = (union [] []; inter [] []; diff [] [])

let main () = ()
let _ = main ()