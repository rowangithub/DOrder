let app (b:int) (f:int->unit) x = f x
let check (x:int) (y:int) = assert (x = y)
let main a b = app (4 * a + 2 * b) (check (4 * a + 2 * b)) (4 * a + 2 * b)