(*
 * 'a llist(i, n): a list of lists such that
 *     1. the sum of the lengths of lists in the list of lists is i, and
 *     2. the length of the list is n.
 *)
datatype 'a llist with (nat, nat) =
    lnil(0,0)
  | {i:nat, j:nat, n:nat} lcons (i+j, n+1) of 'a list(i) * 'a llist (j, n)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (xs as x :: xs', ys as y :: ys') =
    if x < y then x :: merge (xs', ys) else y :: merge (xs, ys')
withtype {i:nat,j:nat} <i+j> => int list(i) * int list(j) -> int list(i+j)

fun initList [] = lnil
  | initList (l as [x]) = lcons (l, lnil)
  | initList (x1 :: x2 :: xs) =
    let
        val l = (if x1 < x2 then [x1, x2] else [x2, x1]: int list(2))
    in
        lcons (l, initList xs)
    end
withtype {i:nat} <i> => int list(i) -> [n:nat] int llist(i, n)

fun mergeList (ls as lnil) = ls
  | mergeList (ls as lcons (_, lnil)) = ls
  | mergeList (lcons (l1, lcons (l2, ls))) = lcons (merge (l1, l2), mergeList ls)
withtype {i:nat,n:nat} <n> => int llist(i,n) -> 
         [n':nat | (n' < n /\ 2 <= n) \/ (n' = n /\ n < 2)] int llist (i, n')

fun mergeAll lnil = []
  | mergeAll (lcons (l, lnil)) = l
  | mergeAll (ls as lcons (_, lcons (_, _))) = mergeAll (mergeList ls)
withtype {i:nat,n:nat} <n> => int llist(i,n) -> int list(i)

fun mergesort l = mergeAll (initList l)
withtype {i:nat} <> => int list(i) -> int list(i)
