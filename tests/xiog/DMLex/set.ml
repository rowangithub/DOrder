(*
 * An implementation set operations in DML
 * The code is taken from Objective Caml library
 *)

val compare: ('a).'a * 'a -> int

datatype 'a tree with (nat,nat) =
  Empty(0,0)
| {sl:nat,sr:nat,hl:nat,hr:nat}
  Node(sl+1+sr,1+max(hl,hr)) of
  'a tree(sl,hl) * 'a * 'a tree(sr,hr) * int(1+max(hl,hr))

fun('a)
  height Empty = 0
| height (Node (_, _, _, h)) = h
withtype {s:nat,h:nat} <> => 'a tree(s,h) -> int(h)

fun('a) create l x r =
  let
      val hl = height l
      and hr = height r
  in
      if hl >= hr then Node (l, x, r, hl + 1) else Node (l, x, r, hr + 1)
  end
withtype {sl:nat,hl:nat}
         'a tree(sl,hl) -> 'a -> {sr:nat,hr:nat} <> =>
         'a tree(sr,hr) -> 'a tree (sl+1+sr,1+max(hl,hr))

fun('a) bal l x r =
  let
      val hl = height l
      and hr = height r
  in
      if hl > hr + 2 then
        case l of
          Node (ll, lv, lr, _) =>
            if height ll >= height lr then
              create ll lv (create lr x r)
            else
              case lr of
                Node (lrl, lrv, lrr, _) =>
                  create (create ll lv lrl) lrv (create lrr x r)
      else if hr > hl + 2 then
        case r of
          Node (rl, rv, rr, _) =>
            if height rr >= height rl then
              create (create l x rl) rv rr
            else
              case rl of
                Node (rll, rlv, rlr, _) =>
                  create (create l x rll) rlv (create rlr rv rr)
      else create l x r
  end
withtype {sl:nat,hl:nat} 'a tree(sl,hl) -> 'a ->
         {sr:nat,hr:nat} <> => 'a tree(sr,hr) -> [h':pos] 'a tree (sl+1+sr,h')

fun('a)
  join_aux d l x r =
    case bal l x r of
       t' as Node (l', x', r', _) =>
         let
             val d' = abs (height l' - height r')
         in
             if d' <= 2 then t'
             else if d' < d then join_aux d' l' x' r'
                  else create l x r
         end
withtype {n:int | n > 2} <n> => int(n) ->
         {sl:nat,hl:nat} 'a tree(sl,hl) -> 'a ->
         {sr:nat,hr:nat} 'a tree(sr,hr) -> [h':pos] 'a tree (sl+1+sr,h')

fun('a)
  join l x r =
    let
        val d = abs (height l - height r)
    in
       if d <= 2 then create l x r else join_aux d l x r
    end
withtype {sl:nat,hl:nat} 'a tree(sl,hl) -> 'a ->
         {sr:nat,hr:nat} <> => 'a tree(sr,hr) -> [h':pos] 'a tree (sl+1+sr,h')

fun('a) merge t1 t2 =
  case (t1, t2) of
    (Empty, _) => t2
  | (_, Empty) => t1
  | (Node (l1, v1, r1, h1), Node (l2, v2, r2, h2)) =>
    bal l1 v1 (bal (merge r1 l2) v2 r2)
withtype {sl:nat,hl:nat} <sl> => 'a tree(sl, hl) ->
         {sr:nat,hr:nat} 'a tree(sr, hr) -> [h:nat] 'a tree (sl+sr,h)

fun('a) concat t1 t2 =
  case (t1, t2) of
    (Empty, _) => t2
  | (_, Empty) => t1
  | (Node (l1, v1, r1, h1), Node (l2, v2, r2, h2)) =>
    join l1 v1 (join (concat r1 l2) v2 r2)
withtype {sl:nat,hl:nat} <sl> => 'a tree(sl, hl) ->
         {sr:nat,hr:nat} 'a tree(sr, hr) -> [h:nat] 'a tree (sl+sr,h)

fun('a)
  split x Empty = (Empty, NONE, Empty)
| split x (Node (l, v, r, _)) = 
  let
      val c = compare (x, v)
  in
      if c = 0 then (l, SOME v, r)
      else if c < 0 then
        let
            val (ll, vl, rl) = split x l
            val r' = join rl v r
        in
            (ll, vl, r')
        end        
      else
        let
            val (lr, vr, rr) = split x r
            val l' = join l v lr
        in
            (l', vr, rr)
        end
  end
withtype 'a -> {s:nat,h:nat} <s> => 'a tree (s, h) ->
         [sl:nat,hl:nat,a:sb,sr:nat,hr:nat | sl+a+sr = s]
         ('a tree(sl,hl) * 'a option(a) * 'a tree(sr,hr))


(* Implementation of the set operations *)

val empty = Empty

fun('a) isEpmty t = case t of Empty => true | Node _ => false
withtype {s:nat,h:nat} 'a tree(s,h) -> bool(s=0)

fun('a)
  mem x Empty = false
| mem x (Node (l, v, r, _)) =
  let
      val c = compare (x, v)
  in
      if c = 0 then true
      else if c < 0 then mem x l
           else mem x r
  end
withtype {s:nat,h:nat} <s> => 'a -> 'a tree(s,h) -> bool

fun('a) singleton x = Node (Empty, x, Empty, 1)
withtype 'a -> 'a tree(1,1)

fun('a)
  add x Empty = singleton x
| add x (t as Node (l, v, r, _)) =
  let
      val c = compare (x, v)
  in
      if c = 0 then t
      else if c < 0 then bal (add x l) v r
           else bal l v (add x r)
  end
withtype 'a -> {s:nat,h:nat} <s> => 'a tree(s,h) ->
         [s':pos,h':pos | s <= s' <= s+1] 'a tree(s',h')

fun('a)
  remove x Empty = Empty
| remove x (Node (l, v, r, _)) =
  let
      val c = compare (x, v)
  in
      if c = 0 then merge l r
      else if c < 0 then bal (remove x l) v r
           else bal l v (remove x r)
  end
withtype 'a -> {s:nat,h:nat} <s> => 'a tree(s,h) ->
         [s':nat,h':nat | s' <= s <= s'+1] 'a tree(s',h')

fun('a) union s1 s2 =
  case (s1, s2) of
    (Empty, _) => s2
  | (_, Empty) => s1
  | (Node (l1, v1, r1, h1), Node (l2, v2, r2, h2)) =>
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else
        let
            val (l2, _, r2) = split v1 s2
        in
            join (union l1 l2) v1 (union r1 r2)
        end
    else
      if h1 = 1 then add v1 s2 else
        let
            val (l1, _, r1) = split v2 s1
        in
            join (union l1 l2) v2 (union r1 r2)
        end
withtype {s1:nat,h1:nat} 'a tree(s1,h1) ->
         {s2:nat,h2:nat} <s1+s2> => 'a tree(s2,h2) ->
         [s:nat, h:nat] 'a tree (s,h)

fun('a) inter s1 s2 =
  case (s1, s2) of
    (Empty, _) => Empty
  | (_, Empty) => Empty
  | (Node (l1, v1, r1, _), _) =>
    let
        val (l2, ov2, r2) = split v1 s2
    in
        case ov2 of
          NONE => concat (inter l1 l2) (inter r1 r2)
        | SOME _ => join (inter l1 l2) v1 (inter r1 r2)
    end
withtype {s1:nat,h1:nat} <s1> => 'a tree(s1,h1) ->
         {s2:nat,h2:nat} 'a tree(s2,h2) ->
         [s:nat, h:nat | s <= s1, s <= s2] 'a tree (s,h)

fun('a) diff s1 s2 =
  case (s1, s2) of
    (Empty, _) => s2
  | (_, Empty) => s1
  | (Node (l1, v1, r1, _), _) =>
    let
        val (l2, ov2, r2) = split v1 s2
    in
        case ov2 of
          NONE => join (diff l1 l2) v1 (diff r1 r2)
        | SOME _ => concat (diff l1 l2) (diff r1 r2)
    end
withtype {s1:nat,h1:nat} <s1> => 'a tree(s1,h1) ->
         {s2:nat,h2:nat} 'a tree(s2,h2) ->
         [s:nat, h:nat | s1 <= s + s2, s2 <= s + s1] 'a tree (s,h)

datatype 'a treelist with (nat, nat) =
  Tnil (0,0)
| {s:nat,h:nat,n:nat,t:nat}
  Tcons (s+n,s) of 'a tree(s,h) * 'a treelist(n,t)

fun('a)
  compare_aux Tnil Tnil = 0
| compare_aux Tnil _ = ~1
| compare_aux _ Tnil = 1
| compare_aux (ts1 as Tcons (t1, ts1')) (ts2 as Tcons (t2, ts2')) =
  case (t1, t2) of
    (Node (Empty, v1, r1, _), Node (Empty, v2, r2, _)) =>
    let
        val c = compare (v1, v2)
    in
        if c = 0 then
	  compare_aux (Tcons (r1, ts1')) (Tcons (r2, ts2'))
        else c
    end
  | (Node (l1, v1, r1, _), _) =>
    let
        val h = 1 + height r1
        val ts1 = Tcons (l1, Tcons (Node (Empty, v1, r1, h), ts1'))
    in
        compare_aux ts1 ts2
    end
  | (_, Node (l2, v2, r2, _)) =>
    let
        val h = 1 + height r2
        val ts2 = Tcons (l2, Tcons (Node (Empty, v2, r2, h), ts2'))
    in
       compare_aux ts1 ts2
    end
withtype {s1:nat,n1:nat} 'a treelist(s1, n1) ->
         {s2:nat,n2:nat} <n1+n2,s1+s2> => 'a treelist(s2, n2) -> int

fun('a) equal s1 s2 =
  let
      val c = compare_aux (Tcons (s1, Tnil)) (Tcons (s2, Tnil))
  in
      c = 0
  end
withtype 'a tree -> 'a tree -> bool

fun('a)
    iter f Empty = ()
  | iter f (Node (l, v, r, _)) =
    let
        val _ = iter f l
        val _ = f v
    in
        iter f r
    end
withtype ('a -> unit) -> {s:nat,h:nat} <s> => 'a tree(s,h) -> unit

fun('a,'b) fold  f s accu =
  case s of
    Empty => accu
  | Node (l, v, r, _) => fold f l (f v (fold f r accu))
withtype ('a -> 'b -> 'b) -> {s:nat,h:nat} <s> => 'a tree(s,h) -> 'b -> 'b

fun('a)
  cardinal Empty = 0
| cardinal (Node (l, _, r, _)) = cardinal l + 1 + cardinal r
withtype {s:nat,h:nat} <s> => 'a tree(s,h) -> int(s)

fun('a)
  elements_aux accu Empty = accu
| elements_aux accu (Node (l, v, r, _)) =
  elements_aux (v :: elements_aux accu r) l
withtype {n:nat} 'a list(n) ->
         {s:nat,h:nat} <s> => 'a tree(s,h) -> 'a list(n+s)

fun('a) elements t = elements_aux [] t
withtype {s:nat,h:nat} <> => 'a tree(s,h) -> 'a list(s)

fun('a) min_elt (Node (l, v, _, _)) =
  case l of Empty => v | Node _ => min_elt l
withtype {s:pos,h:pos} <s> => 'a tree(s,h) -> 'a

fun('a) max_elt (Node (_, v, r, _)) =
  case r of Empty => v | Node _ => max_elt r
withtype {s:pos,h:pos} <s> => 'a tree(s,h) -> 'a

val choose = min_elt
