let f (x:int) g h = g (h x)

let h (x:int) = x

let g y = y + 1

let main a = 
	if (a > 0) then
		assert (f a g h > 1)
	else ()