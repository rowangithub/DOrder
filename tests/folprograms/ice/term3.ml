let rec loop x y = 
	if x < 100 then
		loop (x+y) y
	else x, y

let main x y = 
  let x, y = 
		if (y > 0) then 
			loop x y
		else x, y in
	assert (y<=0 || (y>0 && x>=100))
	

let _ = main 2 2
let _ = main 2 (-2)
let _ = main (-2) 2