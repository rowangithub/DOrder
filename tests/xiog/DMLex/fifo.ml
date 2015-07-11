(* fifo.dml
 *
 * Applicative fifos
 *
 * Translated by Hongwei Xi (June 21, 2001) from SML library
 *
 *)

val reverse: ('a). {n:nat} 'a list(n) -> 'a list(n)
val length: ('a). {n:nat} 'a list(n) -> int(n)
val list_app: ('a). {n:nat} ('a -> unit) -> 'a list(n) -> unit
val list_map: ('a,'b). {n:nat} ('a -> 'b) -> 'a list(n) -> 'b list(n)
val list_foldl: ('a,'b). ('a * 'b -> 'b) -> 'b -> {n:nat} 'a list(n) -> 'b
val list_foldr: ('a,'b). ('a * 'b -> 'b) -> 'b -> {n:nat} 'a list(n) -> 'b

datatype 'a fifo with nat =
    {m:nat,n:nat} Q(m+n) of 'a list (m) * 'a list (n)

val empty = Q ([], [])

fun('a)
    isEmpty (Q ([], [])) = true
  | isEmpty (Q (_ :: _, _)) = false
  | isEmpty (Q (_, _ :: _)) = false
withtype {l:nat} <> => 'a fifo(l) -> bool (l = 0)

fun('a) enqueue (Q (fs, rs), x) = Q (fs, x :: rs)
withtype {l:nat} <> => 'a fifo(l) * 'a -> 'a fifo(l+1)

fun('a)
    dequeue (Q (f :: fs, rs)) = (Q (fs, rs), f)
  | dequeue (Q ([], rs)) = case reverse (rs) of f :: fs => (Q (fs, []), f)
withtype {l:pos} <> => 'a fifo(l) -> 'a fifo(l-1) * 'a

fun('a)
  delete (Q (fs, rs), p) =
    let
	fun doRear [] = abort ("delete: no such element")
          | doRear (r :: rs) = if p (r) then rs else r :: (doRear rs)
        withtype {n:nat} <n> => 'a list(n) -> [n':nat | n'+1=n] 'a list(n')

	fun doFront ([], rs) =
	    let val fs = doRear (reverse rs) in (fs, []) end
          | doFront (f :: fs, rs) =
              if p (f) then (fs, rs)
	      else let val (fs, rs) = doFront (fs, rs) in (f :: fs, rs) end
        withtype {m:nat,n:nat} <m> =>
	         'a list(m) * 'a list(n) ->
                 [m':nat,n':nat | m'+n'+1=m+n] ('a list(m') * 'a list(n'))
    in
	Q (doFront (fs, rs))
    end
withtype {l:pos} <> => 'a fifo(l) * ('a -> bool) -> 'a fifo(l-1)

fun('a)
    peek (Q (f :: _, _)) = SOME f
  | peek (Q ([], rs as _ :: _)) = (case reverse (rs) of f :: _ => SOME f)
  | peek (Q ([], [])) = NONE
withtype {l:nat} <> => 'a fifo(l) -> 'a option(l<>0)

fun('a)
    head (Q (f :: _, _)) = f
  | head (Q ([], rs as _ :: _)) = (case reverse (rs) of f :: _ => f)
withtype {l:pos} <> => 'a fifo(l) -> 'a

fun('a) qlength (Q (fs, rs)) = (length fs) + (length rs)
withtype {l:nat} <> => 'a fifo(l) -> int(l)

fun('a) contents (Q (fs, rs)) = fs @ reverse (rs)
withtype {l:nat} <> => 'a fifo(l) -> 'a list(l)

fun('a)
  qapp f (Q (fs, rs)) = 
    let val _ = list_app f fs in list_app f (reverse rs) end
withtype <> => ('a -> unit) -> 'a fifo -> unit

fun('a,'b)
  qmap f (Q (fs, rs)) = Q (list_map f fs, list_map f (reverse rs))
withtype <> => ('a -> 'b) -> {l:nat} 'a fifo(l) -> 'b fifo(l)

fun('a,'b)
  qfoldl f b (Q (fs, rs)) = list_foldr f (list_foldl f b fs) rs
withtype <> => ('a * 'b -> 'b) -> 'b -> {l:nat} 'a fifo(l) -> 'b

fun('a, 'b)
  qfoldr f b (Q (fs, rs)) = list_foldr f (list_foldl f b rs) fs
withtype <> => ('a * 'b -> 'b) -> 'b -> {l:nat} 'a fifo(l) -> 'b
