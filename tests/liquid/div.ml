qualifier NONZERO(x) = not(x = 0);;
qualifier POS(x) = 0 < x;;
qualifier NNEG(x) = 0 <= x;;

let abs x = if x < 0 then (0 - x) else x in
let trunc i j =
  let ai = abs i in
  let aj = abs j in
    if aj <= ai then j else (fun k -> k) aj
in
let s = trunc 0 0 in
let t = trunc 10 1 in
  s;;
