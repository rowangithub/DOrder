fun fact_cont n k =
  if n = 0 then k 1 else fact_cont (n-1) (fn res => n * k (res)

fun fact n = fact_cont n (fn res => res)