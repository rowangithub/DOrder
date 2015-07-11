(*
let test1 k =
  if eq_int 0 (k + k) then 0
  else if ge_int (k + k) 2 then 0 else 1
withtype {k:nat} int(k) -> int(0)
;;

let test2 k =
  if eq_int k 0 then 0
  else if eq_int (k mod 2) 0 then
         if lt_int k 2 then 1 else 0
       else 0
withtype {k:nat} int(k) -> int(0)
;;
*)

datatype 'a brauntree with nat = (* height and size *)
    L(0)
  | {m:nat}{n:nat |  n <= m <= n+1}
    B(m+n+1) of 'a * 'a brauntree(m) * 'a brauntree(n)
;;

let rec size = function
    L -> 0
  | B(_, l, r) -> 1 + size(l) + size(r)
withtype {n:nat} 'a brauntree(n) -> int(n)
;;

exception Illegal_argument ;;

let make_brauntree x l r =
  let m = size l and n = size r in
    if le_int n m && le_int m (n+1) then B(x, l, r)
    else raise Illegal_argument
withtype 'a -> 'a brauntree -> 'a brauntree -> 'a brauntree
;;

let rec diff k = function
    L -> 0
  | B(_, l, r) ->
    if eq_int k 0 then 1
    else if eq_int (k mod 2) 1 then diff (k/2) l else diff (k/2 - 1) r

withtype {k:nat}{n:nat | k <= n <= k+1} int(k) -> 'a brauntree(n) -> int(n-k)
;;

let rec size = function
    L -> 0
  | B(_, l, r) -> let n = size r in 1 + n + n + diff n l
withtype {n:nat} 'a brauntree(n) -> int(n)
;;

let rec copy2 x k =
  if eq_int k 0 then (B(x, L, L), L)
  else let (s, t) = copy2 x ((k - 1) / 2) in
       if eq_int (k mod 2)  0 then (B(x, s, s), B(x, s, t))
       else (B(x, s, t), B(x, t, t))
withtype {k:nat} 'a -> int(k) -> 'a brauntree(k+1) * 'a brauntree(k)
;;

let copy x k = let (_, t) = copy2 x k in t
withtype {k:nat} 'a -> int(k) -> 'a brauntree(k)
;;

let make_brauntree x l r =
  let m = size(l) and n = size(r) in
    if le_int n m && lt_int m (n+1) then B(x, l, r)
    else raise Illegal_argument
withtype 'a -> 'a brauntree -> 'a brauntree -> 'a brauntree
;;

(*
sort sb == {a:int | 0 <= a <= 1 } ;;

datatype 'a braunlist with (sb, nat) =
    {m:nat} Bnil(0,m)
  | {b:sb}{m:nat}{n:nat | n <= m <= n+1-b}
    Bcons(m-n+b,m) of 'a brauntree(m) * 'a braunlist(b, n)
;;

exception Split_empty ;;

let rec split k = function
    Bnil -> (Bnil, Bnil)
  | Bcons(t, ts') as ts ->
    if k = 0 then (Bnil, ts)
    else let (ts1, ts2) = split (k-1) ts' in (Bcons(t, ts1), ts2)
withtype {b:sb}{m:nat} int -> 'a braunlist(b, m) ->
         [b1:sb][b2:sb][n:nat | n <= m /\ b1+b2+m-n<=1]
         ('a braunlist(b1, m) * 'a braunlist(b2, n))
;;

let rec mk_braunlist xs ts1 ts2 =
    match (xs, ts1, ts2) with
      (x :: xs, Bcons(t1, ts1), Bcons(t2, ts2)) ->
      Bcons(B(x, t1, t2), mk_braunlist xs ts1 ts2)
    | ([], _, _) -> Bnil
withtype {b1:sb}{b2:sb}{n1:nat}{n2:nat | n2+b1 <= n1 /\ b1+b2+n1-n2 <= 1}
         'a list -> 'a braunlist(b1, n1) -> 'a braunlist(b2, n2) ->
         [b:sb][n:nat | n <= n1+n2+1 <= n+1] 'a braunlist(b, n)
;;
*)
