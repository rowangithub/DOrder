let f n i = 
	(assert (0 <= i && i < n); 0)

let update (n:int) (i:int) (x:int) a (j:int) = 
	if j > i-1 && j <= i then 
		x
	else 
		a j

let rec init (x:int) i n (a:int->int) = 
	if i >=n then a
	else
		let u = update n i x a
		in init x (i+1) n u
		
let main m ith x =
	let a = f m in
	let y = init x 0 m a in
	if ith >= 0 && ith < m then
		assert (y ith >= x)
	else () 


(*let _ = main 2 0 4*)
(*let _ = main 2 1 6*)
let _ = main 3 1 1
	

		