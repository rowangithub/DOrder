let rec loopb k i n = 
	if i < n then	loopb k (i+1) n 
	else ()
	
let rec loopc k i n = 
	if i < n then
		(assert (1 <= k); loopc k (i+1) n)
	else ()
	
let rec loopa k l n = 
	if k < n then
		(loopb k l n; loopc k l n; loopa (k+1) l n)
	else ()	

let main l n =
	if (l>0) then 
		loopa 1 l n
	else ()