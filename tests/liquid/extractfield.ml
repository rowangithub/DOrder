type t = {x: int; y: int};;
qualif XGEY(r): r.y <= r.x;;
qualif POS(x): 0 < x;;
qualif GTZ(k): z < k;;
qualif EQAX(m): m = a.x;;
qualif EQAY(m): m = a.y;;
let a = {x = 2; y = 1} in
let z = a.y in
let w = a.x in
  w;;
