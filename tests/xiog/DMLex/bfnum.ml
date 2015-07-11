datatype 'a Tree with nat =
  E(0) | {sl:nat, sr:nat} T(1+sl+sr) of 'a * 'a Tree(sl) * 'a Tree(sr)

datatype 'a TreeList with (nat, nat) =
  Tnil(0,0) | {n:nat,s:nat,s':nat} Tcons(s+s',n+1) of 'a Tree(s) * 'a TreeList(s',n)

fun('a) refTree E = E
  | refTree (T (x, a, b)) = T (ref 0, refTree a, refTree b)
withtype {s:nat} 'a Tree(s) -> (int ref) Tree(s)

fun('a) revTreeList ts =
  let
      fun aux (Tnil, res) = res
        | aux (Tcons(t, ts), res) = aux (ts, Tcons (t, res))
      withtype {n:nat,s:nat,n':nat,s':nat} <n> =>
               'a TreeList(s,n) * 'a TreeList(s',n') -> 'a TreeList(s+s',n+n')
  in
      aux (ts, Tnil)
  end
withtype {n:nat,s:nat} <> => 'a TreeList (s,n) -> 'a TreeList (s,n)


fun('a)
    bfLab i Tnil Tnil = ()
  | bfLab i Tnil (ts' as Tcons _) = bfLab i (revTreeList ts') Tnil
  | bfLab i (Tcons(E, ts)) ts' = bfLab i ts ts'
  | bfLab i (Tcons(T (r, a, b), ts)) ts' =
    (r := i; bfLab (i+1) ts (Tcons (b, Tcons (a, ts'))))
withtype {n:nat,s:nat,n':nat,s':nat} <s+s',n+n',n'> =>
         int -> (int ref) TreeList(s,n) -> (int ref) TreeList(s',n') -> unit

fun intTree E = E
  | intTree (T (r, a, b)) = T (!r, intTree a, intTree b)
withtype {n:nat} <n> => (int ref) Tree (n) -> int Tree (n)

fun('a) bfnum t =
  let
      val t = refTree t
      val _ = bfLab 1 (Tcons(t,Tnil)) Tnil
  in
      intTree t
  end
withtype {n:nat} 'a Tree (n) -> int Tree (n)
