let rec unb1 arr2 m n i j =
  let k = n - 1 in
  if j < k then
    if Bigarray.Array2.get arr2 0 j < 0 then
      let z = i + 1 in
        unb2 arr2 m n z j
    else
      let w = j + 1 in
        unb1 arr2 m n 0 w
  else false
and unb2 arr2 m n i j =
  if i < m then
    if Bigarray.Array2.get arr2 i j < 0 then
      let p = i + 1 in
        unb2 arr2 m n p j
    else
      let r = j + 1 in
        unb1 arr2 m n 0 r
  else true

let driver =
  (Random.self_init ();
  let dim1 = Random.int 10 in
  let dim1 = dim1 + 1 in
  let dim2 = Random.int 10 in
  let dim2 = dim2 + 1 in
  let arr =
    Bigarray.Array2.create
      Bigarray.int Bigarray.c_layout
      dim1 dim2 in
  let m = Bigarray.Array2.dim1 arr in
  let n = Bigarray.Array2.dim2 arr in
    unb1 arr m n 0 1)
