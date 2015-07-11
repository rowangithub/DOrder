(* iterate.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

val badArg: ('a). string * string -> 'a

fun('a) iterate f cnt init =
  let 
      fun iter (n, v) = if n = 0 then v else iter (n - 1, f v)
      withtype {n:nat} <n> => int(n) * 'a -> 'a
  in
      if cnt < 0 then badArg ("iterate","count < 0")
      else iter (cnt,init)
  end
withtype <> => ('a -> 'a) -> int -> ('a -> 'a)
        
fun('a) repeat f cnt init =
  let
      fun iter (cnt,n,v) = if n = cnt then v else iter(cnt,n+1,f(n,v))
      withtype {n:int,cnt:int | 0 <= n <= cnt} <cnt-n> =>
               int(cnt) * int(n) * 'a -> 'a
  in
      if cnt < 0 then badArg ("repeat","count < 0")
      else iter (cnt,0,init)
  end
withtype <> => (int * 'a -> 'a) -> int -> ('a -> 'a)

fun('a) for_up f (n, stop, inc, v) =
    if n > stop then v else for_up f (n+inc, stop, inc, f (n,v))
withtype (int * 'a -> 'a) ->
        {stop:int,n:int,inc:pos | n <= stop+inc} <stop+inc-n> =>
         int(n) * int(stop) * int(inc) * 'a -> 'a

fun('a) for_down f (n, stop, inc, v) =
    if n < stop then v else for_down f (n+inc, stop, inc, f (n,v))
withtype (int * 'a -> 'a) ->
        {stop:int,n:int,inc:neg | n >= stop+inc} <n-stop-inc> =>
         int(n) * int(stop) * int(inc) * 'a -> 'a

fun('a) for f (start, stop, inc) =
    if start < stop
	then if inc <= 0 then badArg ("for","inc <= 0 with start < stop")
	     else fn v => for_up f (start, stop, inc, v)
    else if stop < start
	     then if inc >= 0 then badArg ("for","inc >= 0 with start > stop")
		  else fn v => for_down f (start, stop, inc, v)
	 else fn v => f (start, v)
withtype <> => (int * 'a -> 'a) ->
         {inc:int | inc <> 0} int * int * int(inc) -> ('a -> 'a)

