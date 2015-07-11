qual P(x): 0 < x;;
qual N(x): x < 0;;

? letrec makelist = fun e -> fun n ->
  if n = 0 then
    []
  else
    e::(makelist e (n - 1))
in
let a = makelist 10 8 in
let b = makelist -10 1 in
  a;;
