let rec forall (xs:int list) f = 
	match xs with
		| x :: xs -> 
			if (f x > 0) then forall xs f
			else 0
		| [] -> 1

let f x = 
	if (x > 0) then 1
	else 0
	

let main () = 
	let xs= [1; 2; 3] in
	let res = forall xs f in
	assert (res = 1) 
	
let _ = main ()	