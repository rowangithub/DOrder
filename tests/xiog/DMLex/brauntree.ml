(* An implementation of Braun trees in DML *)

datatype 'a brauntree with nat =
    L(0)
  | {m:nat, n:nat | n <= m <= n+1}
    B(m+n+1) of 'a * 'a brauntree(m) * 'a brauntree(n)

fun('a)
    diff (k, L) = 0
  | diff (k, B(_, l, r)) =
    if k = 0 then 1
    else if k % 2 = 1 then diff (k/2, l) else diff (k/2 - 1, r)
withtype {k:nat, n:nat | k <= n <= k+1} <n> =>
         int(k) * 'a brauntree(n) -> int(n-k)

fun('a)
    size (L) = 0
  | size (B(_, l, r)) =
    let val n = size r in 1 + n + n + diff (n, l) end
withtype {n:nat} <n> => 'a brauntree(n) -> int(n)

