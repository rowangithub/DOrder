let c (q:int) = ()
let b x (q:int) : unit = x 1
let a (x:int->unit) (y:int->unit) q = (assert (q = 0); x 0; y 0) 
let rec f n x q = if n <= 0 then x q else a x (f (n-1) (b x)) q
let s n q = f n c q
let main n = s n 0

let _ = main 5