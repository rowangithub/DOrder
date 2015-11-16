let f (g:int->int->int) x y = g (x + 1) (y + 1)

let rec unzip x (k:int->int->int)=
  if x=0 then
    k 0 0
  else
    unzip (x - 1) (f k)
		
let rec zip x y =
  if x = 0 then (assert (y = 0); 0)
  else (assert (y <> 0); 1 + zip (x - 1) (y - 1))
    
let main n = unzip n zip

let _ = main 0