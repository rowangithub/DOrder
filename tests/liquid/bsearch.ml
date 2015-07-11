let rec bsearch key vec lo hi =
	if lo < (hi-1)  then
		let hl = hi + lo in
		let m = hl / 2  in
 		let x = Array.get vec m in
		let diff = key - x in
			(if diff < 0 then bsearch key vec lo (m-1)
			else if 0 < diff then bsearch key vec (m+1) hi
			else if key = x then m else -1)
	else -1 
  
	
let main vec key = 
	let sv = Array.length vec in
  let index = bsearch key vec 0 (sv-1) in
	if (index != -1) then
		assert (Array.get vec index = key) 
	else ()
	
let _ = main [|1; 2; 3; 4; 5|] 3


