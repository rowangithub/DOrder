let rec loop (d1:int) (d2:int) (d3:int) x1 x2 x3 = 
	let c1 = Random.bool () in
	let c2 = Random.bool () in
	if (x1 > 0 && x2 > 0 && x3 > 0) then
		if (c1) then
			loop d1 d2 d3 (x1-1) x2 x3
		else if (c2) then
			loop d1 d2 d3 x1 (x2-1) x3
		else loop d1 d2 d3 x1 x2 (x3 - 1)
	else assert ((x1 = 0 || x2 = 0) || x3 = 0)
	
	
let main () = 
	let (d1, d2, d3) = (1, 1, 1) in
	let x1 = Random.int 10 in
	let x2 = Random.int 10 in
	let x3 = Random.int 10 in
	loop 1 1 1 x1 x2 x3 


let _ = main ()