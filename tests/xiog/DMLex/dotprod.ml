fun dotprod_aux (v1, v2, i, n, sum) =
  if i = n then sum
  else dotprod_aux (v1, v2, i+1, n, sum + sub(v1, i) * sub(v2, i))
withtype {n:nat, i:nat | i <= n} <n-i> =>
int array(n) * int array(n) * int(i) * int(n) * int  -> int

fun dotprod(v1, v2) = dotprod_aux (v1, v2, 0, arraysize v1, 0)
withtype {n:nat} <> => int array(n) * int array(n) -> int