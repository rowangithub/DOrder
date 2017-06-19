let f x g :unit= g(x+1)
let h (z:int) (y:int) = assert (y>z)
let main n = if n>=0 then f n (h n) else ()