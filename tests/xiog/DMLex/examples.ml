fun ack m n =
  if m = 0 then n+1 else if n = 0 then ack (m-1) 1 else ack (m-1) (ack m (n-1))
withtype {i:nat,j:nat} <i,j> => int(i) -> int(j) -> [k:nat] int(k)

fun loop (j, k) =
  if (k < j) then
    if (k > 1) then loop (j - k, k / 2)
    else 0
  else j + k
withtype {a:nat,b:pos} <max(0, a-b)> =>
         int(a) * int(b) -> int

datatype Nat with nat =
  Z(0) | {n:nat} S(n+1) of Nat(n)

fun('a)
  R Z u v = u
| R (S n) u v = v n (R n u v)
withtype {n:nat} <n> =>
         Nat(n) -> 'a ->
         (Nat -> 'a -> 'a) -> 'a

fun f91 (x) =
  if (x <= 100) then f91 (f91 (x + 11)) else x - 10
withtype {i:int} <max(0, 101-i)> => 
         int(i) -> [j:int | (i<=100 /\ j=91) \/ (i>=101 /\ j=i-10)] int(j)

fun('a)
  qs cmp xs =
    case xs of
      [] => []
    | x :: xs' => par cmp (x, [], [], xs')
withtype ('a * 'a -> bool) -> {n:nat} <n,0> => 'a list(n) -> 'a list(n)

and('a)
  par cmp (x, l, r, xs) =
    case xs of
      [] => qs cmp l @ (x :: qs cmp r)
    | x' :: xs' => if cmp(x', x) then par cmp (x, x' :: l, r, xs')
                 else par cmp (x, l, x' :: r, xs')
withtype ('a * 'a -> bool) ->
         {p:nat,q:nat,r:nat} <p+q+r,r+1> =>
         'a * 'a list(p) * 'a list(q) * 'a list(r) -> 'a list(p+q+r+1)

datatype pattern with nat =
  Empty(0) (* empty string matches Empty *)
| Char(1) of char (* "c" matches Char (c) *)
| {i:nat,j:nat} Plus(i+j+1) of pattern(i) * pattern(j)
  (* cs matches Plus(p1, p2) if cs matches either p1 or p2 *)
| {i:nat,j:nat} Times(i+j+1) of pattern(i) * pattern(j)
  (* cs matches Times(p1, p2) if a prefix of cs matches p1 and
     the rest matches p2 *)
| {i:nat} Star(i+1) of pattern(i)
  (* cs matches Star(p) if cs matches some, possibly 0, copies of p *)

(* 'length' computes the length of a list *)

fun('a)
  length (xs) =
    let
       fun len ([], n) = n
         | len (x :: xs, n) = len (xs, n+1)
       withtype {i:nat,j:nat} <i> => 'a list(i) * int(j) -> int(i+j)
    in
       len (xs, 0)
    end
withtype {i:nat} <> => 'a list(i) -> int(i)

fun acc p cs k =
  case p of
    Empty => k (cs)
  | Char(c) =>
    (case cs of
       [] => false
     | c' :: cs' => if char_eq (c, c') then k (cs') else false)
  | Plus(p1, p2) => if acc p1 cs k then true else acc p2 cs k
  | Times(p1, p2) => acc p1 cs (fn cs' => acc p2 cs' k)
  | Star(p0) =>
    if k (cs) then true
    else acc p0 cs (fn cs' =>
                      if length(cs') = length(cs) then false
                      else acc p cs' k)
withtype {n:nat} pattern(n) ->
         {i:nat} <n, i> => char list(i) -> 
         ({i':nat | i' <= i} char list(i') -> bool) -> bool

(* 'explode' turns a string into a list of characters *)
fun accept p s = acc p (explode s) (fn [] => true | _ :: _ => false)
withtype <> => pattern -> string -> bool

(*
 * 'a llist(i, n): a list of lists such that
 *     1. the sum of the lengths of lists in the list of lists is i, and
 *     2. the length of the list is n.
 *)
datatype 'a llist with (nat, nat) =
    lnil(0,0)
  | {i:nat, j:nat, n:nat} lcons (i+j, n+1) of 'a list(i) * 'a llist (j, n)

fun merge ([], ys) = ys
  | merge (xs, []) = xs
  | merge (xs as x :: xs', ys as y :: ys') =
    if x < y then x :: merge (xs', ys) else y :: merge (xs, ys')
withtype {i:nat,j:nat} <i+j> => int list(i) * int list(j) -> int list(i+j)

fun initList [] = lnil
  | initList (l as [x]) = lcons (l, lnil)
  | initList (x1 :: x2 :: xs) =
    let
        val l = (if x1 < x2 then [x1, x2] else [x2, x1]: int list(2))
    in
        lcons (l, initList xs)
    end
withtype {i:nat} <i> => int list(i) -> [n:nat] int llist(i, n)

fun mergeList (ls as lnil) = ls
  | mergeList (ls as lcons (_, lnil)) = ls
  | mergeList (lcons (l1, lcons (l2, ls))) = lcons (merge (l1, l2), mergeList ls)
withtype {i:nat,n:nat} <n> => int llist(i,n) -> 
         [n':nat | (n' < n /\ 2 <= n) \/ (n' = n /\ n < 2)] int llist (i, n')

fun mergeAll lnil = []
  | mergeAll (lcons (l, lnil)) = l
  | mergeAll (ls as lcons (_, lcons (_, _))) = mergeAll (mergeList ls)
withtype {i:nat,n:nat} <n> => int llist(i,n) -> int list(i)

fun mergeSort l = mergeAll (initList l)
withtype {i:nat} <> => int list(i) -> int list(i)

(*
 * 'a list(n) is the type for lists of length n
 * [] is assigned the type 'a list(0)
 * op:: is assigned the type {n:nat} 'a * 'a list(n) -> 'a list(n+1)
 *)
fun f [] k = k []
  | f (c :: cs) k = (* note that #"c" represents a constant character c *)
    if char_eq (c, #"(") then f cs (fn cs' => (f cs' k) + 1)
    else if char_eq (c, #")") then k (cs) - 1 else ~1
withtype {n:nat} <n> => char list(n) -> ({n':nat | n' <= n} char list(n') -> int) -> int
