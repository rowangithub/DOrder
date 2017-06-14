let rec zip x y =
  if x = 0 then (assert (y = 0); x)
  else (assert (y <> 0); 1 + zip (x - 1) (y - 1))
    
 
let rec map x =
  if x = 0 then x else 1 + map (x - 1)

let main n =
  assert (map (zip n n) = n)
	
	
let _ = main 5
let _ = main 0