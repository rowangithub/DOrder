let rec loop x n = 
	if x < n then 	
  		loop (x+1) n
  	else assert (x = n)
  	

let main () = 
	let x = 0 in
 	let n = Random.int 10 in
 	loop x n
 	
let _ = main ()