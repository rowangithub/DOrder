let f x = x + 2

let g (i:int) (h:int->int) = h i

let main y = 
	let t = y in
	let z = g y f in
	assert (z > t)