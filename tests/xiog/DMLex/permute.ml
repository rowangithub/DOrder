fun ('a)
  conslist (x, []) = []
| conslist (x, xs :: xss) = (x :: xs) :: conslist (x, xss)
withtype {n:nat,p:nat} <p> =>
         'a * ('a list(n)) list(p) -> ('a list(n+1)) list(p)

fun ('a)
  perm [] = [[]]
| perm (x :: xs) = perm_aux (x, [], xs, conslist (x, perm xs))
withtype {n:nat} <n,0> => 'a list(n) -> ('a list(n)) list

and ('a)
  perm_aux (x, ys, [], res) = res
| perm_aux (x, ys, z :: zs, res) =
  perm_aux (x, z :: ys, zs,
            conslist (z, perm (ys @ (x :: zs))) @ res)
withtype {p:nat,q:nat,n:pos | p+q=n-1} <n-1,q+1> =>
         'a * 'a list(p) * 'a list(q) * ('a list(n)) list ->
         ('a list(n)) list

