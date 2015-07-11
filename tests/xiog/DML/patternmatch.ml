datatype pattern with nat =
  Empty(0) | Const(1) of char
| {l:nat}{r:nat} Plus(l+r+1) of pattern(l) * pattern(r)
| {l:nat}{r:nat} Times(l+r+1) of pattern(l) * pattern(r)
| {n:nat} Star(n+1) of pattern(n)
;;

let rec pm pat cont str =
  match pat with  
    Empty -> cont str
  | Const(ch) -> begin
    match str with
      [] -> false
    | ch' :: str' -> if (ch = ch') then cont str' else false
    end
  | Plus(p1, p2) -> if pm p1 cont str then true else pm p2 cont str
  | Times(p1, p2) -> pm p1 (fun str -> pm p2 cont str) str
  | Star(p) ->
    if cont str then true
    else pm p (fun str' ->
                 if list_length(str') = list_length(str) then false
                 else pm pat cont str') str
withtype {size:nat} pattern(size) -> {s:nat} ({n:nat | n <= s} char list(n) -> bool) -> char list(s) -> bool
;;

let pattern_match pat str = pm pat (function [] -> true | _ :: _ -> false) str
;;