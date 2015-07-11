let max (x:int) y =
  if x > y then x else y

(*let rec sum k =
  if k < 0 then 0 else
    let s = sum (k-1) in
      s + k*)

let foldn n (b:int) f =
  let rec loop i c =
    if i < n then loop (i+1) (f i c) else c in
    loop 0 b 
		

let arraymax a =
  let am l m = max (Array.get a l) m in
    foldn (Array.length a) 0 am

(*let arraytest a =
  let vec = Array.make (Random.int 40)  0 in
    arraymax vec*)
		
let main m ith = 
	let vec = Array.make m 0 in
	let r  = arraymax vec in
	if (0 <= ith && ith < m) then
		assert (r >= vec.(ith))
	else ()
	
let _ = main 5 3
			
