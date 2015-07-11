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

fun acc i cs k p =
  case p of
    Empty => k (i, cs)
  | Char(c) =>
    (case cs of
       [] => false
     | c' :: cs' => if char_eq(c, c') then k (i-1, cs') else false)
  | Plus(p1, p2) => if acc i cs k p1 then true else acc i cs k p2
  | Times(p1, p2) => acc i cs (fn (i', cs') => acc i' cs' k p2) p1
  | Star(p0) =>
    if k (i, cs) then true
    else acc i cs (fn (i', cs') =>
                   if i' = i then false else acc i' cs' k p) p0
withtype {i:nat} int(i) -> char list(i) ->
         ({i':nat | i' <= i} int(i') * char list(i') -> bool) ->
         {n:nat}  <n, i> => pattern(n) -> bool

(* 'explode' turns a string into a list of characters *)
fun accept p s =
  let
      val cs = explode s
      val i = length cs
  in
      acc i cs (fn (i, _) => (i = 0)) p
  end
withtype <> => pattern -> string -> bool
