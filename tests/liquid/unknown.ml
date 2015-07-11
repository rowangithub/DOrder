qualif POS(x): 0 <= x
qualif NEG(x): x < 0

let ys = [1; 2; -3]

let abs x = if 0 < x then x else 0 - x

let k = List.map abs ys
