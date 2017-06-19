let add x1 x2 = x1 + x2
let twice x (n:int) (f:int->int) = (assert (x >= 0); f (f x))
let main n = assert (twice 0 n (add n) >= 2 * n)