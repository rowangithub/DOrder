let add x1 x2 = x1 + x2
let comp (x:int) (n:int) (f:int->int) g = (assert (x >= 0); f (g x))
let main n = assert (comp 0 n (add n) (add n) >= 2 * n)

let _ = main 5