(*
USED: PLDI2011 as apply
*)

let apply f x = f x
let g y z = assert (y=z)
let rec k n = apply (g n) n; k(n+1)
let main i = k 0
