let lock st = assert (st=0); 10
let unlock st = assert (st=10); 0
let f n st : int= if n > 0 then lock (st) else st
let g n st : int = if n > 0 then unlock (st) else st
let main n = assert ((g n (f n 0)) = 0)

let _ = main 3
let _ = main 2
let _ = main 1
let _ = main 0
let _ = main (-1)
let _ = main (-2) 
let _ = main (-3)