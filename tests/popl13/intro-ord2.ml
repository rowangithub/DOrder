let rec app (a:int) (f:int->unit) x = if Random.bool () then app a f (x + 1) else f x
let check (x:int) (y:int) = assert (x <= y)
let main i = app i (check i) i

let _ = main 5