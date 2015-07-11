let app (a:int) (f:int->(int->unit)->unit) (a:int) (g:int->unit) = f a g
let f x (a:int) (k:int->unit) = k x
let check (x:int) (y:int) = assert (x = y)
let main a b = app (4 * a + 2 * b) (f (4 * a + 2 * b)) (4 * a + 2 * b) (check (4 * a + 2 * b))

let _ = main 1 1