(* An implementation of binary heap in DML *)

datatype tree with nat =
    {n:nat} Node(n) of int(n) * int * treelist(n)

and treelist with nat =
    Tempty(0)
  | {m:nat, n:nat | m >= n} Tcons(m+1) of tree(m) * treelist(n)

datatype heap with nat =
    Hempty(0)
  | {m:nat, n:nat | n = 0 \/ m+1 < n} Hcons(m+1) of tree(m) * heap(n)

fun rank (Node(r, _, _)) = r
withtype {n:nat} tree(n) -> int(n)

fun root (Node(_, x, _)) = x
withtype {n:nat} tree(n) -> int

fun link (t1 as Node(r, x1, ts1), t2 as Node(_, x2, ts2)) =
    if (x1 <= x2) then Node(r+1, x1, Tcons(t2, ts1))
    else Node(r+1, x2, Tcons(t1, ts2))
withtype {r:nat} tree(r) * tree(r) -> tree(r+1)

fun insTree (t, Hempty) = Hcons(t, Hempty)
  | insTree (t, ts as Hcons(t', ts')) =
    if rank t < rank t' then Hcons(t, ts) else insTree (link (t, t'), ts')
withtype {r:nat, rh:nat | rh = 0 \/ r < rh}
         tree(r) * heap(rh) -> [rh':nat | r < rh'] heap(rh')

fun insert (x, hp) = insTree (Node(0, x, Tempty), hp)
withtype int * heap -> [n:nat | n > 0] heap(n)

fun merge (hp1, Hempty) = hp1
  | merge (Hempty, hp2) = hp2
  | merge (hp1 as Hcons(t1, hp1'), hp2 as Hcons(t2, hp2')) =
    if rank t1 < rank t2 then Hcons(t1, merge(hp1', hp2))
    else if rank t1 > rank t2 then Hcons(t2, merge(hp1, hp2'))
    else let val hp = merge(hp1', hp2') in insTree (link (t1, t2), hp) end
withtype {m:nat, n:nat} heap(m) * heap(n) ->
         [l:nat | (n = 0 /\ l = m) \/ (m = 0 /\ l = n) \/ (l >= min(m, n) > 0)] heap(l)

(* exception Heap_is_empty *)

exception Empty_heap

fun removeMinTree Hempty = raise Empty_heap
  | removeMinTree (Hcons(t, Hempty)) = (t, Hempty)
  | removeMinTree (Hcons(t, hp)) =
    let
	val (t', hp') = removeMinTree hp
    in
	if root t < root t' then (t, hp) else (t', Hcons(t, hp'))
    end
withtype {n:nat} heap(n) -> [r:nat][l:nat | l = 0 \/ l >= n > 0] (tree(r) * heap(l))

fun findMin hp =
    let val (t, _) = removeMinTree hp in root t end
withtype heap -> int

fun to_heap (hp, Tempty) = hp
  | to_heap (hp, Tcons(t, ts)) = to_heap (Hcons(t, hp), ts)
withtype {m:nat, n:nat | m = 0 \/ m > n} heap(m) * treelist(n) -> heap

fun deleteMin hp =
  let
      val (Node(_, x, ts), hp) = removeMinTree hp
  in
      merge (to_heap (Hempty, ts), hp)
  end
withtype heap -> heap
