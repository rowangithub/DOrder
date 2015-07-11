qualif POS(x): 0 < x
qualif NEG(x): x < 0

type 'a lst = Nil | Cons of 'a * 'a lst

let f x = x + 1

let l = Cons (f, Nil)
let _ = l
