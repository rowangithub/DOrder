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
(* empty tuple <> is used since 'length' is not recursive *)

fun acc p cs k = 
  case p of
    Empty => k (cs)
  | Char(c) =>
    (case cs of
       [] => false
     | c' :: cs' => if (char_eq (c, c')) then k (cs') else false)
  | Plus(p1, p2) => (* in this case, k is used for backtracking *)
    if acc p1 cs k then true else acc p2 cs k
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
 * pattern'(n, p):
 *   1. n stands for the size of the pattern.
 *   2. p means a string has at least length p
 *      if it matches the pattern
 *)
datatype pattern' with (nat, nat) = 
  Nil' (0, 1) (* no string matches Nil' *)
| Empty'(0, 0)
| Char'(1, 1) of char
| {n1:nat,n2:nat,l1:pos,l2:pos}
  Plus'(n1+n2+1,min(l1, l2))
  of pattern'(n1,l1) * pattern'(n2,l2)
| {n1:nat,n2:nat,l1:nat,l2:nat | l1+l2 > 0}
  Times'(n1+n2+1,l1+l2)
  of pattern'(n1,l1) * pattern'(n2,l2)
| {n:nat,l:pos} Star'(n+1,0) of pattern'(n,l)

fun delta Empty = Empty'
  | delta (Char _) = Nil'
  | delta (Plus (p1, p2)) =
    (case (delta p1, delta p2) of (Nil', Nil') => Nil' | _ => Empty')
  | delta (Times (p1, p2)) =
    (case (delta p1, delta p2) of (Empty', Empty') => Empty' | _ => Nil')
  | delta (Star p) = Empty'
withtype {n:nat} <n> => pattern(n) -> pattern'

fun norm Empty = Nil'
  | norm (Char c) = Char' c
  | norm (Plus (p1, p2)) = Plus' (norm p1, norm p2)
  | norm (Times (p1, p2)) =
    let
        val p1' = norm p1 and p2' = norm p2
        val dp1' = delta p1 and dp2' = delta p2
    in
        Plus' (Plus' (Times' (dp1', p2'), Times' (p1', dp2')), Times' (p1', p2'))
    end
  | norm (Star p) = let val p' = norm p in Times' (p', Star' p') end

withtype {n:nat} <n> => pattern(n) -> [n':nat,p:pos] pattern'(n',p)

fun acc p cs k =
  case p of
    Nil' => false
  | Empty' => k (cs)
  | Char' (c) => (case cs of [] => false | c' :: cs' => if char_eq (c, c') then k (cs') else false)
  | Plus' (p1, p2) => if acc p1 cs k then true else acc p2 cs k
  | Times' (p1, p2) => acc p1 cs (fn cs' => acc p2 cs' k)
  | Star'(p0) => if k (cs) then true else acc p0 cs (fn cs' => acc p cs' k)
withtype {n:nat,l:nat} pattern'(n, l) -> {i:nat} <n, i> => char list(i) ->
         ({i':nat | i'+l <= i} char list(i') -> bool) -> bool

fun accept p s =
  let
      val cs = explode s
  in
      if acc (delta p) cs (fn [] => true | _ :: _ => false) then true
      else acc (norm p) cs (fn [] => true | _ :: _ => false)
  end
withtype <> => pattern -> string -> bool
