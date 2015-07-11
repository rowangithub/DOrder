let succ (b:int) (f:int->unit) x = f (x + 1)
let rec app x (f:int->unit) = if Random.bool () then app x (succ x f) else f x
let check (x:int) y = assert (x <= y)
let main i = app i (check i)

let _ = main 5