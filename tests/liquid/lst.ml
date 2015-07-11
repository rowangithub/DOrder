

type 'a lst = Nil | Cons of 'a * 'a lst

let empty = Nil
let _ = empty

let pos = Cons (1, empty)
let _ = pos


let abs x = 
  if x > 0 then x else (0 - x)

