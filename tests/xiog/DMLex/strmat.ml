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
