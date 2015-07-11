let f l i = (assert (0 <= i && i < l); l-i)

let rec array_max (n:int) i (a:int->int) (m:int) =
  if i >= n then
    m
  else
    let x = a i in
    let z = if x>m then x else m in
		array_max n (i+1) a z
		
let main (n:int) (ith:int) =
	let a = f n in
	let m = array_max n 0 a 0 in
	if ith >= 0 && ith < n then
		assert (a ith <= m)
	else ()
		
		
let _ = main 2 0

