let lock st = assert (st=0); 10
let unlock st = assert (st=10); 0
let f n st : int= if n > 0 then lock (st) else st
let g n st : int = if n > 0 then unlock (st) else st
let main n = assert ((g n (f n 0)) = 0)