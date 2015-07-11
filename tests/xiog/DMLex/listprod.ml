val reverse: ('a).{n:nat} 'a list(n) -> 'a list(n)

fun('a,'b)
  lp_aux_1 (A, Bs, res) =
    case Bs of
      [] => res
    | B :: Bs => lp_aux_1 (A, Bs, (A, B) :: res)
withtype {b:nat, n:nat} <b> =>
         'a * 'b list(b) * ('a * 'b) list(n) -> ('a * 'b) list(n+b)

fun('a,'b)
  lp_aux_2 (As, Bs, res) =
    case As of
      [] => res
    | A :: As => lp_aux_2 (As, Bs, lp_aux_1 (A, Bs, res))
withtype {a:nat, b:nat, n:nat} <a> =>
         'a list(a) * 'b list(b) * ('a * 'b) list(n) -> ('a * 'b) list

fun('a,'b)
  listprod (As, Bs) = reverse (lp_aux_2 (As, Bs, []))
withtype {a:nat,b:nat} <> => 'a list(a) * 'b list(b) -> ('a * 'b) list



