(* squalif POS(x): 0 <= x
   squalif NEG(x): x < 0 *)

type 'a tree = | Empty
               | Node of 'a tree * 'a * 'a tree

let empty = Empty
let _ = empty

let one_node = Node (Empty, 1, Empty)
let _ = one_node

let two_node = Node (one_node, 2, Empty)
let _ = two_node

let three_node = Node (Empty, -3, two_node)
let _ = three_node

let four_node = Node (one_node, 4, two_node)
let _ = four_node

let five_node = Node (empty, 5, four_node)
let _ = five_node

let abs x =
  if x > 0 then x else (0 - x)

let rec map f t =
  match t with
    | Empty -> Empty
    | Node (l, t, r) -> Node (map f l, f t, map f r)

let _ = map abs three_node
