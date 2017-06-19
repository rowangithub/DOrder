let rec loop x y n = 
		let y = 
			if (x <= n) then y + 1 
			else if (x >= n+1) then y - 1 
			else assert false in
		if (y < 0) then 
			if n >= 0 then 
				if y = (-1) then assert (x < 2*n + 3)
				else ()
			else ()
		else loop (x+1) y	n 


let main n =
	let x = 0 in
  let y = 0 in
 
  if(n < 0) then ()
  else 
		loop x y n