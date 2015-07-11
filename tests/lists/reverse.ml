let rec reverseinner (xs:int list) (acc:int list) =
  match xs with
      [] -> acc
    | x::xs' -> reverseinner xs' (x::acc)

let reverse (xs:int list) = reverseinner xs []

let rec make_list n =
  if n = 0
  then []
  else n :: make_list (n-1)

let hd (xs:int list) =
  match xs with
      [] -> (assert false)
    | x::xs' -> x

let main len =
  let xs = make_list len in
  if len > 0 then hd (reverse xs)
  else 0
		
let _ = main 2