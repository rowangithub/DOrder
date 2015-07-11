datatype Nat with nat =
  Z(0) | {n:nat} S(n+1) of Nat(n)

fun('a)
  R Z u v = u
| R (S n) u v = v n (R n u v)
withtype {n:nat} <n> =>
         Nat(n) -> 'a ->
         (Nat -> 'a -> 'a) -> 'a
