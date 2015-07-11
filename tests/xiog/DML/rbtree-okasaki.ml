type key == int;;

sort color == {a:int | 0 <= a <= 1};;

datatype tree with (color, nat, nat, nat) = (* color, black height, violation, size *)
    E(0, 0, 0, 0)
  | {cl:color}{cr:color}{bh:nat}{sl:nat}{sr:nat}
    B(0, bh+1, 0, sl+sr+1) of tree(cl, bh, 0, sl) * key * tree(cr, bh, 0, sr)
  | {cl:color}{cr:color}{bh:nat}{sl:nat}{sr:nat}
    R(1, bh, cl+cr, sl+sr+1) of tree(cl, bh, 0, sl) * key * tree(cr, bh, 0, sr)
;;

exception Item_already_exists;;

let balance = function
    (R(R(a, x, b), y, c), z, d) -> R(B(a, x, b), y, B(c, z, d))
  | (R(a, x, R(b, y, c)), z, d) -> R(B(a, x, b), y, B(c, z, d))
  | (a, x, R(R(b, y, c), z, d)) -> R(B(a, x, b), y, B(c, z, d))
  | (a, x, R(b, y, R(c, z, d))) -> R(B(a, x, b), y, B(c, z, d))
(*
  | (a, x, b) -> B(a, x, b)
*)
  | ((R(B _, _, B _) as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, B _) as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, B _) as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, B _) as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, B _) as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((R(B _, _, B _) as tl), z, (E as tr)) -> B(tl, z, tr)

  | ((R(B _, _, E) as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, E) as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, E) as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, E) as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((R(B _, _, E) as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((R(B _, _, E) as tl), z, (E as tr)) -> B(tl, z, tr)

  | ((R(E, _, B _) as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(E, _, B _) as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((R(E, _, B _) as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(E, _, B _) as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((R(E, _, B _) as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((R(E, _, B _) as tl), z, (E as tr)) -> B(tl, z, tr)

  | ((R(E, _, E) as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(E, _, E) as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((R(E, _, E) as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((R(E, _, E) as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((R(E, _, E) as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((R(E, _, E) as tl), z, (E as tr)) -> B(tl, z, tr)

  | ((B _ as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((B _ as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((B _ as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((B _ as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((B _ as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((B _ as tl), z, (E as tr)) -> B(tl, z, tr)

  | ((E as tl), z, (R(B _, _, B _) as tr)) -> B(tl, z, tr)
  | ((E as tl), z, (R(B _, _, E) as tr)) -> B(tl, z, tr)
  | ((E as tl), z, (R(E, _, B _) as tr)) -> B(tl, z, tr)
  | ((E as tl), z, (R(E, _, E) as tr)) -> B(tl, z, tr)
  | ((E as tl), z, (B _ as tr)) -> B(tl, z, tr)
  | ((E as tl), z, (E as tr)) -> B(tl, z, tr)
withtype {cl:color}{cr:color}{bh:nat}{vl: nat}{vr:nat | vl+vr <= 1}{sl:nat}{sr:nat}
         tree(cl, bh, vl, sl) * key * tree(cr, bh, vr, sr) ->
         [c:color] tree(c, bh+1, 0, sl+sr+1)
;;

let insert x t =
  let rec ins = function
      E -> R(E, x, E)
    | B(a, y, b) ->
      if x < y then balance(ins a, y, b)
      else if y < x then balance(a, y, ins b)
           else raise Item_already_exists
    | R(a, y, b) ->
      if x < y then R(ins a, y, b)
      else if y < x then R(a, y, ins b)
           else raise Item_already_exists
   withtype {c:color}{bh:nat}{s:nat}
            tree(c, bh, 0, s) ->  [c':color][v:nat | v <= c] tree(c', bh, v, s+1) in
   match ins t with
     R(a, y, b) -> B(a, y, b)
(*
   | t -> t
*)
   | B _ as t -> t
   | E as t -> t
withtype {c:color}{bh:nat}{s:nat} key -> tree(c, bh, 0, s) -> [bh:nat] tree(0, bh, 0, s+1)
;;
