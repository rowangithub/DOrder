qualif NNEG(v): v >= 0 

let boo x = x + 10

let abs x = 
  if x > 0 then x else (0 - x) 

let v1 = abs 1

let v2 = abs (-1)

let rec sum1 k = 
  if k < 0 then 0 else 
    let s = sum1 (k-1) in
    s + k

let rec sum2 k = 
  if k <= 0 then 0 else
    let s = sum2 (k-1) in
    s + k

let rec sum3 k = 
  if k = 0 then 0 else
    let s = sum3 (k-1) in
    s + k

let v3 = sum1 100

let v4 = sum2 100

let v5 = sum3 100

let v6 = boo (-10)

let max x y = 
  if x > y then x else y

let v7 = max 10 100
let v8 = max 10 (-100)


