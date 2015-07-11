datatype Ord = Zero | Succ of Ord | Lim of Nat -> Ord
and Nat = Z | S of Nat

fun add (alpha, Zero) = alpha
  | add (alpha, Succ beta) = Succ (add (alpha, beta))
  | add (alpha, Lim f) = Lim (fn x => add (alpha, f(x)))
withtype Ord * Ord -> Ord
