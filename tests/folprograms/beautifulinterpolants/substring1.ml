let rec loop (k:int) i j = 
	if (i < k) then
		loop k (i+1) (j+1)
	else assert (j < 101)

let main k from =	
	if (k >= 0 && k <= 100 && from >= 0 && from <= k) then
		let i = from in
		let j = 0 in
		loop k i j
	else ()

let _ = main 20 5
let _ = main 9 0
let _ = main 0 9
let _ = main 5 20
let _ = main 200 200
let _ = main (-1) (-1)