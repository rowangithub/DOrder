qualif POS(x): 0 < x
qualif NEG(x): x < 0

type ('a, 'b) thing = Empty | AThing of 'a * ('a, 'b) thing | BThing of 'b * ('a, 'b) thing

let a = AThing (12, Empty)
let _ = a

let b = BThing (-12, Empty)
let _ = b

let c = AThing (12, b)
let _ = c
