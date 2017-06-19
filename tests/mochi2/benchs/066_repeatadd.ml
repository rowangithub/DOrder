let add x1 x2 = x1 + x2
let rec repeat (k:int) (x:int) (n:int) (f:int->int) = 
	if k <= 0 then x else f (repeat (k - 1) x n f)
let main n k = if n >= 0 && k > 0 then assert (repeat k 0 n (add n) >= n) else ()
