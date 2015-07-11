fun dp (a, b) =
  let
     val n = arraysize a
     fun loop (i, sum) =
       if i = n then sum else loop (i+1, sum + sub (a, i) * sub (b, i))
     withtype {i:nat | i <= n} <n-i> => int(i) * int -> int
  in
     loop (0, 0)
  end  
withtype {n:nat} int array(n) * int array(n) -> int

fun ack m n =
  if m = 0 then n+1
  else
      let
	  fun ackm n = ack (m-1) n
          withtype {n:nat} <> => int(n) -> [a:nat] int(a)
      in
	  if n = 0 then ackm 1 else ackm (ack m (n-1))
      end
withtype {m:nat} int(m) -> {n:nat} <m, n> => int(n) -> [a:nat] int(a)

(*
fun dp {n:nat} (a: int array(n), b: int array(n)): int =
  let
     val n = arraysize a
     fun loop {i:nat | i < n} (i: int(i), sum: int) =
       if i = n then sum else loop (i+1, sum  + sub (a, i) * sub (b, i))
  in
     loop (0, 0)
  end  

fun zip {n:nat} (xs: 'a list(n) , ys: 'b list(n)): ('a * 'b) list(n) =
  case (a, b) of
    ([], []) => []
  | (x :: xs, y :: ys) => (x, y) :: zip (xs, ys)

fun ack {i:nat} (m:int(i)) {j:nat} (n:int(j)): int =
  if m = 0 then n+1
  else if n = 0 then ack (m-1) 1
       else ack (m-1) (ack m (n-1))
withtype {i:nat} int(i) -> {j:nat} int(j) -> int

fun ack (m:int(i)) (n:int(j)): int =
  if m = 0 then n+1
  else if n = 0 then ack (m-1) 1
       else ack (m-1) (ack m (n-1))
withtype {i:nat} int(i) -> {j:nat} int(j) -> int

*)