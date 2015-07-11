(* An implementation of AVL trees in DML *)

datatype order = LESS | EQUAL | GREATER

val compare: ('a).'a * 'a -> order

datatype 'a avltree with (nat, nat) =
  E(0, 0)
| {lh:nat,ls:nat,rh:nat,rs:nat | rh <= lh <= rh+1}
  Bl(1+lh, 1+ls+rs) of
  int(1+lh) * 'a * 'a avltree(lh, ls) * 'a avltree(rh, rs)
| {lh:nat,ls:nat,rh:nat,rs:nat | lh <= rh <= lh+1}
  Br(1+rh, 1+ls+rs) of
  int(1+rh) * 'a * 'a avltree(lh, ls) * 'a avltree(rh, rs)

fun('a)
    height E = 0
  | height (Bl (h, _, _, _)) = h
  | height (Br (h, _, _, _)) = h
withtype {h:nat,s:nat} <> => 'a avltree(h, s) -> int(h)

fun('a) left_rotate (e, l, r) =
    case l of
	Bl (lh, le, ll, lr) =>
	    let
		val lrh = height (lr)
	    in
		Br (lrh+2, le, ll, Bl (lrh+1, e, lr, r))
	    end
      | Br (lh, le, ll, lr) =>
	    let
		val llh = height ll
		and lrh = height lr
	    in
		if llh < lrh then (* rh = llh: deep rotation *)
		    case lr of
			Bl (_, lre, lrl, lrr) =>
			    Br (lh, lre, Bl (llh+1, le, ll, lrl), Br (lh-1, e, lrr, r))
		      | Br (_, lre, lrl, lrr) =>
			    Br (lh, lre, Bl (llh+1, le, ll, lrl), Br (lh-1, e, lrr, r))
		else Br (lrh+2, le, ll, Bl (lrh+1, e, lr, r))
	    end
withtype {ls:nat,rh:nat,rs:nat} <> =>
         'a * 'a avltree(rh+2, ls) * 'a avltree(rh, rs) ->
	 [h':nat | rh+2 <= h' <= rh+3] 'a avltree (h',1+ls+rs)

fun('a) right_rotate (e, l, r) =
    case r of
	Bl (rh, re, rl, rr) =>
	    let
		val rlh = height rl
		and rrh = height rr
	    in
		if rlh > rrh then (* lh = rrh: deep rotation *)
		    case rl of
			Bl (_, rle, rll, rlr) =>
			    Bl (rh, rle, Bl (rh-1, e, l, rll), Br (rrh+1, re, rlr, rr))
		      | Br (_, rle, rll, rlr) =>
			    Bl (rh, rle, Bl (rh-1, e, l, rll), Br (rrh+1, re, rlr, rr))
		else Bl (rlh+2, re, Br (rlh+1, e, l, rl), rr)
	    end
      | Br (rh, re, rl, rr) =>
	    let
		val rlh = height (rl)
	    in
		Bl (rlh+2, re, Br (rlh+1, e, l, rl), rr)
	    end
withtype {lh:nat,ls:nat,rs:nat} <> =>
         'a * 'a avltree(lh, ls) * 'a avltree(lh+2, rs) ->
	 [h':nat | lh+2 <= h' <= lh+3] 'a avltree (h',1+ls+rs)

exception Item_is_found

fun('a)
    insert (key, E) = Bl (1, key, E, E)
  | insert (key, t) =
    let
	val (h, e, l, r) =
	    (case t of
		 Bl (h, e, l, r) => (h, e, l, r)
	       | Br (h, e, l, r) => (h, e, l, r) :
            [lh:nat,ls:nat,rh:nat,rs:nat |
	     lh-1<=rh<=lh+1,h=1+max(lh,rh),s=1+ls+rs]
            (int(1+max(lh,rh)) * 'a * 'a avltree(lh,ls) * 'a avltree(rh,rs)))
    in
	case compare(key, e) of
	    LESS =>
		let
		    val l' = insert (key, l)
		    val lh' = height l'
		    val rh = height r
		in
		    if lh' <= rh then Br (rh+1, e, l', r)
		    else if lh' <= rh+1 then Bl (lh'+1, e, l', r)
			 else left_rotate (e, l', r)
		end
	  | GREAER =>
		let
		    val lh = height l
		    val r' = insert (key, r)
		    val rh' = height r'
		in
		    if rh' <= lh then Bl (lh+1, e, l, r')
		    else if rh' <= lh+1 then Br (rh'+1, e, l, r')
			 else right_rotate (e, l, r')
		end
	  | EQUAL => raise Item_is_found
    end
withtype {h:nat,s:nat} <s> => 'a * 'a avltree(h,s) ->
         [h':nat | h <= h' <= h+1] 'a avltree(h',s+1)
