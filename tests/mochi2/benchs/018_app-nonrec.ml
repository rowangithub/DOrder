let apply (b:int) (f: int->unit) x = f x
let check (x:int) (y:int) = assert (x = y)
let main (n:int) = apply n (check n) n