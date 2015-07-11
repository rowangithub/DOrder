qualif POS(x): 0 < x
qualif NNEG(x): 0 <= x
qualif NEG(x): x < 0

let lst = [1; 2; 3]

let _ = lst

let k = match lst with
  | [] -> 0
  | x :: _ -> (fun z -> z) x

let _ = (fun z -> z) k
