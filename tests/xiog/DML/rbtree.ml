type key == int
and 'a entry == int * 'a
and answer == key option
;;

sort color == {a:int | 0 <= a <= 1};;

type order = LESS | EQUAL | GREATER;;

datatype 'a dict with (color, nat, nat, nat) =
  Empty(0, 0, 0, 0)
| {cl:color}{cr:color}{bh:nat}{sl:nat}{sr:nat}
  Black(0, bh+1, 0, sl+sr+1) of 'a entry * 'a dict(cl, bh, 0, sl) * 'a dict(cr, bh, 0, sr)
| {cl:color}{cr:color}{bh:nat}{rhl:nat}{rhr:nat}{sl:nat}{sr:nat}
  Red(1, bh, cl+cr+rhl+rhr, sl+sr+1) of 'a entry * 'a dict(cl, bh, rhl, sl) * 'a dict(cr, bh, rhr, sr)
;;

(* refine must be a full refinement
refine 'a list with nat = nil(0) | {n:nat} cons(n+1) of 'a * 'a list(n)
;;

retype Closure of closure list(n) * lambda_expr(n)
;;
*)

let compare(x,y) = let z = x - y in if z < 0 then LESS else if z > 0 then GREATER else EQUAL
;;

let lookup dict key = begin
    let rec lk = function
	Empty -> None
      | Red tree -> lk' tree
      | Black tree -> lk' tree
    withtype 'a dict -> answer

    and lk' ((key1, datum1), left, right) =
        (match compare(key,key1) with
           EQUAL -> Some(key1)
         | LESS -> lk left
         | GREATER -> lk right)
    withtype 'a entry * 'a dict * 'a dict -> answer
    in lk dict
    end
withtype 'a dict -> key -> answer
;;

let restore_right = function
      (e, Red lt, Red((_,Red _,_) as rt)) -> Red(e, Black lt, Black rt)
    | (e, Red lt, Red((_,_,Red _) as rt)) -> Red(e, Black lt, Black rt)
    | (e, (Empty as l), Red(re, Red(rle, rll, rlr), rr)) ->
      Black(rle, Red(e, l, rll), Red(re, rlr, rr))
    | (e, (Black _ as l), Red(re, Red(rle, rll, rlr), rr)) ->
      Black(rle, Red(e, l, rll), Red(re, rlr, rr))
    | (e, (Empty as l), Red(re, rl, (Red _ as rr))) -> Black(re, Red(e, l, rl), rl)
    | (e, (Black _ as l), Red(re, rl, (Red _ as rr))) -> Black(re, Red(e, l, rl), rr)
    | (e, l, (Red(_, Empty, Empty) as r)) -> Black(e, l, r)
    | (e, l, (Red(_, Black _, Black _) as r)) -> Black(e, l, r)
    | (e, l, (Black _ as r)) -> Black(e, l, r)
withtype {cl:color}{cr:color}{bh:nat}{rhr:nat}{sl:nat}{sr:nat | rhr <= 1}
         'a entry * 'a dict(cl, bh, 0, sl) * 'a dict(cr, bh, rhr, sr) ->
         [c:color] 'a dict(c, bh+1, 0, sl + sr + 1)
;;

let restore_left = function
       (e, Red ((_,Red _,_) as lt), Red rt) -> Red(e, Black lt, Black rt)
     | (e, Red ((_,_,Red _) as lt), Red rt) -> Red(e, Black lt, Black rt)
     | (e, Red(le, (Red _ as ll), lr), (Empty as r)) -> Black(le, ll, Red(e, lr, r))
     | (e, Red(le, (Red _ as ll), lr), (Black _ as r)) -> Black(le, ll, Red(e, lr, r))
     | (e, Red(le, ll, Red(lre, lrl, lrr)), (Black _ as r)) ->
        Black(lre, Red(le, ll, lrl), Red(e, lrr, r))
     | (e, Red(le, ll, Red(lre, lrl, lrr)), (Empty as r)) ->
        Black(lre, Red(le, ll, lrl), Red(e, lrr, r))
     | (e, (Red(_, Empty, Empty) as l), r) -> Black(e, l, r)
     | (e, (Red(_, Black _, Black _) as l), r) -> Black(e, l, r)
     | (e, (Black _ as l), r) -> Black(e, l, r)
withtype {cl:color}{cr:color}{bh:nat}{rhl:nat}{sl:nat}{sr:nat | rhl <= 1}
         'a entry * 'a dict(cl, bh, rhl, sl) * 'a dict(cr, bh, 0, sr) ->
         [c:color] 'a dict(c, bh+1, 0, sl + sr + 1)
;;

exception Item_Is_Found
;;

let insert(dict, ((key,datum) as entry)) =
    let rec ins = function
      (* val ins : 'a dict -> 'a dict  inserts entry *)
      (* ins (Red _) may violate color invariant at root, having red height 1 *)
      (* ins (Black _) or ins (Empty) will be red/black tree *)
      (* ins preserves black height *)
        Empty -> Red(entry, Empty, Empty)
      | Red((key1, datum1) as entry1, left, right) ->
	(match compare (key,key1) with
             EQUAL -> raise Item_Is_Found
           | LESS -> Red(entry1, ins left, right)
           | GREATER -> Red(entry1, left, ins right))
      | Black((key1, datum1) as entry1, left, right) ->
             (match compare (key,key1) with
                 EQUAL -> raise Item_Is_Found
               | LESS -> restore_left(entry1, ins left, right)
               | GREATER -> restore_right(entry1, left, ins right))
    withtype {c:color}{bh:nat}{s:nat}
             'a dict(c, bh, 0, s) ->
             [nc:color][nrh:nat |
                      (c = 0 /\ nrh = 0 /\ nc <= 1) \/ (c = 1 /\ nrh <= 1 /\ nc = 1)]
             'a dict(nc, bh, nrh, s+1)
    in try let dict = ins dict
           in match dict with
                 Red((_, Red _, _) as t) -> Black t (* re-color *)
               | Red((_, _, Red _) as t) -> Black t (* re-color *)
               | Red(_, Black _, Black _) -> dict
               | Red(_, Empty, Empty) -> dict
               | Black _ -> dict
       with Item_Is_Found -> dict
withtype {c:color}{bh:nat}{s:nat}
         'a dict(c, bh, 0, s) * 'a entry ->
         [nc:color][nbh:nat][ns:nat | (nbh = bh \/ nbh = bh + 1) /\ (ns = s \/ ns = s+1)]
         'a dict(nc, nbh, 0, ns)
;;
