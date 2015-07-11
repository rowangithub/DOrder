fun('a) reverse(l) =
let
    fun aux([], ys) = ys
      | aux(x::xs, ys) = aux(xs, x::ys)
    withtype {m:nat,n:nat} 'a list(m) * 'a list(n) -> 'a list(m+n)
in
    aux(l, [])
end
withtype {n:nat} 'a list(n) -> 'a list(n)