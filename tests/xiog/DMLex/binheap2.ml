datatype tree with nat =
    {n:nat, l:nat} Node(n) of int(n) * int * treelist(l, n)

and treelist with (nat, nat) = (* length, rank *)
    Tempty(0, 0)
  | {r:nat, lt:nat, rt:nat | r >= rt}
    Tcons(lt+1, r+1) of tree(r) * treelist(lt, rt)

datatype heap with (nat, nat) = (* length, rank *)
    Hempty(0, 0)
  | {lh:nat, r:nat, rh:nat | rh = 0 \/ r+1 < rh}
    Hcons(lh+1, r+1) of tree(r) * heap(lh, rh)

fun rank (Node(r, _, _)) = r
withtype {n:nat} <> => tree(n) -> int(n)

fun root (Node(_, x, _)) = x
withtype {n:nat} <> => tree(n) -> int

fun link (t1 as Node(r, x1, ts1), t2 as Node(_, x2, ts2)) =
    if (x1 <= x2) then Node(r+1, x1, Tcons(t2, ts1))
    else Node(r+1, x2, Tcons(t1, ts2))
withtype {r:nat} <> => tree(r) * tree(r) -> tree(r+1)

fun insTree (t, Hempty) = Hcons(t, Hempty)
  | insTree (t, ts as Hcons(t', ts')) =
    if rank t < rank t' then Hcons(t, ts) else insTree (link (t, t'), ts')
withtype {r:nat, lh:nat, rh:nat | rh = 0 \/ r < rh} <lh> =>
         tree(r) * heap(lh, rh) -> [lh':nat, rh':nat | r < rh'] heap(lh', rh')

fun insert (x, hp) = insTree (Node(0, x, Tempty), hp)
withtype <> => int * heap -> [lh:nat, rh:nat | rh > 0] heap(lh, rh)

fun merge (hp1, Hempty) = hp1
  | merge (Hempty, hp2) = hp2
  | merge (hp1 as Hcons(t1, hp1'), hp2 as Hcons(t2, hp2')) =
    if rank t1 < rank t2 then Hcons(t1, merge(hp1', hp2))
    else if rank t1 > rank t2 then Hcons(t2, merge(hp1, hp2'))
    else let val hp = merge(hp1', hp2') in insTree (link (t1, t2), hp) end
withtype {lh1:nat, rh1:nat, lh2: nat, rh2:nat} <lh1+lh2> =>
         heap(lh1, rh1) * heap(lh2, rh2) ->
         [lh:nat, rh:nat | (rh2 = 0 /\ rh = rh1) \/
                           (rh1 = 0 /\ rh = rh2) \/
                           (rh >= min(rh1, rh2) > 0)] heap(lh, rh)

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
withtype {lh:nat, rh:nat} <lh> => heap(lh, rh) ->
         [r:nat, lh':nat, rh':nat | rh' = 0 \/ rh'>= rh > 0]
         (tree(r) * heap(lh', rh'))

fun findMin hp =
    let val (t, _) = removeMinTree hp in root t end
withtype <> => heap -> int

fun to_heap (hp, Tempty) = hp
  | to_heap (hp, Tcons(t, ts)) = to_heap (Hcons(t, hp), ts)
withtype {lh:nat, rh:nat, lt:nat, rt:nat | rh = 0 \/ rh > rt} <lt> =>
          heap(lh, rh) * treelist(lt, rt) -> heap

fun deleteMin hp =
  let
      val (Node(_, x, ts), hp) = removeMinTree hp
  in
      merge (to_heap (Hempty, ts), hp)
  end
withtype <> => heap -> heap
