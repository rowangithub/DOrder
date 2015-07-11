datatype tree with nat =
    {n:nat} Node(n) of int(n) * int * treelist(n)

and treelist with nat =
    Tempty(0)
  | {m:nat}{n:nat | m >= n} Tcons(m+1) of tree(m) * treelist(n)
;;

datatype heap with nat =
    Hempty(0)
  | {m:nat}{n:nat | n = 0 \/ m+1 < n} Hcons(m+1) of tree(m) * heap(n)
;;

let rank = function Node(r, _, _) -> r
withtype {n:nat} tree(n) -> int(n)
;;

let root = function Node(_, x, _) -> x
withtype {n:nat} tree(n) -> int
;;

let link (Node(r, x1, c1) as t1)= function
    Node(_, x2, c2) as t2 ->
      if (x1 <= x2) then Node(r+1, x1, Tcons(t2, c1)) else Node(r+1, x2, Tcons(t1, c2))
withtype {r:nat} tree(r) -> tree(r) -> tree(r+1)
;;

let rec insTree t = function
    Hempty -> Hcons(t, Hempty)
  | Hcons(t', ts') as ts ->
    if lt_int (rank t) (rank t') then Hcons(t, ts) else insTree (link t t') ts'
withtype {r:nat}{n:nat | n = 0 \/ r < n}
         tree(r) -> heap(n) -> [l:nat | l > r] heap(l)
;;

let insert x hp = insTree (Node(0, x, Tempty)) hp
withtype int -> [n:nat] heap(n) -> [n:nat | n > 0] heap(n)
;;

let rec merge = function
    (hp1, Hempty) -> hp1
  | (Hempty, hp2) -> hp2
  | (Hcons(t1, hp1') as hp1), (Hcons(t2, hp2') as hp2) ->
    if lt_int (rank t1) (rank t2) then Hcons(t1, merge(hp1', hp2))
    else if gt_int (rank t1) (rank t2) then Hcons(t2, merge(hp1, hp2'))
    else let hp = merge(hp1', hp2') in insTree (link t1 t2) hp
withtype {m:nat}{n:nat} heap(m) * heap(n) ->
         [l:nat | (n = 0 /\ l = m) \/ (m = 0 /\ l = n) \/
                  (l >= min(m, n) > 0)] heap(l)
;;

exception Empty_heap;;

let rec removeMinTree = function
    Hempty -> raise Empty_heap
  | Hcons(t, Hempty) -> (t, Hempty)
  | Hcons(t, hp) ->
    let (t', hp') = removeMinTree hp in
    if root t < root t' then (t, hp) else (t', Hcons(t, hp'))
withtype {n:nat} heap(n) -> [r:nat][l:nat | l = 0 \/ l >= n > 0] (tree(r) * heap(l))
;;

let findMin hp = let (t, _) = removeMinTree hp in root t
withtype {n:nat} heap(n) -> int
;;

let rec to_heap hp = function
    Tempty -> hp
  | Tcons(t, ts) -> to_heap (Hcons(t, hp)) ts
withtype {m:nat}{n:nat | m = 0 \/ m > n} heap(m) -> treelist(n) -> heap
;;

let deleteMin hp =
  let (Node(_, x, ts), hp) = removeMinTree hp
  in merge (to_heap Hempty ts, hp)
withtype heap -> heap
;;
