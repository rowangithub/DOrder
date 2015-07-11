fun ack m n k =
  if m = 0 then k (n+1)
  else if n = 0 then ack (m-1) 1 k
       else ack m (n-1) (fn x => ack (m-1) x)
withtype {i:nat}{j:nat} <i,j> =>
         int(i) -> int(j) -> (nat -> nat) -> nat
