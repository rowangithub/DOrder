(* type key == int *)

sort color = {a:int | 0 <= a <= 1}

datatype rbtree with (color, nat, nat) =
    E(0, 0, 0)
  | {cl:color, cr:color, bh:nat}
    B(0, bh+1, 0) of rbtree(cl, bh, 0) * int * rbtree(cr, bh, 0)
  | {cl:color, cr:color, bh:nat}
    R(1, bh, cl+cr) of rbtree(cl, bh, 0) * int * rbtree(cr, bh, 0)

fun restore (R(R(a, x, b), y, c), z, d) = R(B(a, x, b), y, B(c, z, d))
  | restore (R(a, x, R(b, y, c)), z, d) = R(B(a, x, b), y, B(c, z, d))
  | restore (a, x, R(R(b, y, c), z, d)) = R(B(a, x, b), y, B(c, z, d))
  | restore (a, x, R(b, y, R(c, z, d))) = R(B(a, x, b), y, B(c, z, d))

  | restore (E, x, E) = B (E, x, E)
  | restore (E, x, t2 as B _) = B (E, x, t2)
  | restore (E, x, t2 as R (E, _, E)) = B (E, x, t2)
  | restore (E, x, t2 as R (E, _, B _)) = B (E, x, t2)
  | restore (E, x, t2 as R (B _, _, E)) = B (E, x, t2)
  | restore (E, x, t2 as R (B _, _, B _)) = B (E, x, t2)

  | restore (t1 as B _, x, E) = B (t1, x, E)
  | restore (t1 as B _, x, t2 as B _) = B (t1, x, t2)
  | restore (t1 as B _, x, t2 as R (E, _, E)) = B (t1, x, t2)
  | restore (t1 as B _, x, t2 as R (E, _, B _)) = B (t1, x, t2)
  | restore (t1 as B _, x, t2 as R (B _, _, E)) = B (t1, x, t2)
  | restore (t1 as B _, x, t2 as R (B _, _, B _)) = B (t1, x, t2)

  | restore (t1 as R (E, _, E), x, E) = B (t1, x, E)
  | restore (t1 as R (E, _, E), x, t2 as B _) = B (t1, x, t2)
  | restore (t1 as R (E, _, E), x, t2 as R (E, _, E)) = B (t1, x, t2)
  | restore (t1 as R (E, _, E), x, t2 as R (E, _, B _)) = B (t1, x, t2)
  | restore (t1 as R (E, _, E), x, t2 as R (B _, _, E)) = B (t1, x, t2)
  | restore (t1 as R (E, _, E), x, t2 as R (B _, _, B _)) = B (t1, x, t2)

  | restore (t1 as R (E, _, B _), x, E) = B (t1, x, E)
  | restore (t1 as R (E, _, B _), x, t2 as B _) = B (t1, x, t2)
  | restore (t1 as R (E, _, B _), x, t2 as R (E, _, E)) = B (t1, x, t2)
  | restore (t1 as R (E, _, B _), x, t2 as R (E, _, B _)) = B (t1, x, t2)
  | restore (t1 as R (E, _, B _), x, t2 as R (B _, _, E)) = B (t1, x, t2)
  | restore (t1 as R (E, _, B _), x, t2 as R (B _, _, B _)) = B (t1, x, t2)

  | restore (t1 as R (B _, _, E), x, E) = B (t1, x, E)
  | restore (t1 as R (B _, _, E), x, t2 as B _) = B (t1, x, t2)
  | restore (t1 as R (B _, _, E), x, t2 as R (E, _, E)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, E), x, t2 as R (E, _, B _)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, E), x, t2 as R (B _, _, E)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, E), x, t2 as R (B _, _, B _)) = B (t1, x, t2)

  | restore (t1 as R (B _, _, B _), x, E) = B (t1, x, E)
  | restore (t1 as R (B _, _, B _), x, t2 as B _) = B (t1, x, t2)
  | restore (t1 as R (B _, _, B _), x, t2 as R (E, _, E)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, B _), x, t2 as R (E, _, B _)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, B _), x, t2 as R (B _, _, E)) = B (t1, x, t2)
  | restore (t1 as R (B _, _, B _), x, t2 as R (B _, _, B _)) = B (t1, x, t2)

(*
  | restore (a, x, b) = B(a, x, b)
*)
withtype {cl:color, cr:color, bh:nat, vl:nat, vr:nat | vl+vr <= 1} <> =>
         rbtree(cl, bh, vl) * int * rbtree(cr, bh, vr) ->
         [c:color] rbtree(c, bh+1, 0)

exception Item_already_exists

fun insert (x, t) =
    let
	fun ins (E) = R(E, x, E)
	  | ins (B(a, y, b)) =
	    if x < y then restore(ins a, y, b)
	    else if y < x then restore(a, y, ins b)
                 else raise Item_already_exists
	  | ins (R(a, y, b)) =
	    if x < y then R(ins a, y, b)
	    else if y < x then R(a, y, ins b)
                 else raise Item_already_exists
        withtype {c:color, bh:nat}
                 rbtree(c, bh, 0) -> [c':color][v:nat | v <= c] rbtree(c', bh, v)
    in
	case ins t of
	    R(a, y, b) => B(a, y, b)
	  | t as B _ => t
    end
withtype {c:color, bh:nat} int * rbtree(c, bh, 0) -> [bh':nat] rbtree(0, bh', 0)
