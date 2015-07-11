(* five examples from a paper by Neil Jones et al. *)

fun p (m, n, r) =
  if r > 0 then p (m, r-1, n)
  else if n > 0 then p (r, n-1, m)
       else m
withtype {m:nat,n:nat,r:nat} <m+n+r> =>
         int(m) * int(n) * int(r) -> int

fun('a)
 f(x, y) =
  case y of
    [] => x
  | _ :: y' =>
    (case x of [] => f(y, y') | _ :: x' => f(y, x'))
withtype {m:nat,n:nat} <max(m,n),n> =>
         'a list(m) * 'a list(n) -> 'a list

fun ('a)
  f (xs, []) = g(xs, [])
| f (xs, y :: ys) = f (y :: xs, ys)
withtype {m:nat,n:nat} <1,n> =>
         'a list(m) * 'a list(n) -> 'a list

and ('a)
  g ([], ds) = ds
| g (c :: cs, ds) = g (cs, c :: ds)
withtype {m:nat,n:nat} <0,m> =>
         'a list(m) * 'a list(n) -> 'a list
