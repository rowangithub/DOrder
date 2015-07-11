let succ (b:int) (f:int->unit) x = f (x + 1)

let rec app3 (b:int) (f:int->unit) (a:int) (g:int->(int->unit)->unit) = 
	if Random.bool () then app3 b (succ b f) b g else g b f
	
let app x (b:int) (f:int->unit) = f x

let check (x:int) (y:int) = assert (x <= y)

let main (i:int) = app3 i (check i) i (app i)

let _ = main 2