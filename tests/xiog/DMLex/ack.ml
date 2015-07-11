fun ack m n =
  if m = 0 then n+1
  else if n = 0 then ack (m-1) 1
       else ack (m-1) (ack m (n-1))
withtype {i:nat,j:nat} <i,j> => int(i) -> int(j) -> [k:nat] int(k)

(*
datatype Nat with nat = Z(0) | {n:nat} S(n+1) of Nat(n)
fun ack Z n = S n
  | ack (S m) Z = ack m (S Z)
  | ack (S m) (S n) = ack m (ack (S m) n)
withtype {i:nat} Nat(i) -> {j:nat} <i, j> => Nat(j) -> Nat
*)