type pr = {p1: int; p2: int}

let anno = (fun x -> x) 

let rec bot l b =
  if l < b then {p1 = b; p2 = b}
  else if Random.int 2 = 0 then {p1 = b; p2 = b}
  else bot l (b+1)

let rec bot2 l b =
  if l < b then b else
    bot2 l (b+1) 

let driver =
  let l = Random.int 40 in
  let pp = bot l 0 in
  let _ = anno pp in
  let pa = pp.p1 in
  let pb = pp.p2 in
  let _ = anno pa in
  let _ = anno pb in
  let pb = bot2 l 0 in
  let _ = anno pb in
    ()
