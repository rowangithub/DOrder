datatype 'a heap with nat =
  {n:nat} H(n) of int(n) * [a:nat | a <= n] int(a) * 'a array(n)

fun heapify (h, i) =
    let
	val H (_, size, data) = h
	val left = 2 * i + 1
	val right = 2 * i + 2
	val largest =
	    (if left < size then
		 if sub (data, left) >. sub (data, i) then left else i
	     else i : int[0,n))
	val largest =
	    (if right < size then
		 if sub (data, right) >. sub (data, largest) then right
		 else largest
	     else largest: int[0,n))
    in
	if largest > i then
	    let
		val temp = sub (data, i)
		val _ = update (data, i, sub (data, largest))
		val _ = update (data, largest, temp)
	    in
	        heapify (h, largest)
	    end
	else ()
    end
#withtype {n:nat,i:nat | i < n}  =>
#         float heap(n) * int(i) -> unit


fun buildheap (h) =
    let
	val H(_, size, data) = h
	fun loop (i) =
	    if i >= 0 then
		let val _ = heapify (h, i) in loop (i - 1) end
	    else ()
#	withtype {i:int | i < n} => int(i) -> unit
    in
	loop (size / 2 - 1)
    end	
#withtype {n:nat} float heap(n) -> unit


fun heapsort (h) =
    let
	val H (max, _, data) = h
	val _ = buildheap (h)
	fun loop (i) =
	    if i >= 1 then
		let
		    val temp = sub (data, i)
		    val _ = update (data, i, sub (data, 0))
		    val _ = update (data, 0, temp)
		    val _ = heapify (H (max, i, data), 0)
		in
		    loop (i-1)
		end
	    else ()
#        withtype {i:int | i < n} => int(i) -> unit
    in
	loop (max - 1)
    end
#withtype {n:nat} float heap(n) -> unit

fun print_array	(data, i, j) =
    let
	fun loop (k) =
	    if k < j then
		let
		    val _ = print_float (sub (data, k))
		in
		    loop (k+1)
		end
	    else ()
#        withtype {k:int | i <= k <= j}  => int(k) -> unit
    in
	loop (i)
    end			
#withtype {n:nat, i:int, j:int | 0 <= i <= j <= n}  =>
#         float array(n) * int(i) * int(j) -> unit


fun main () =
    let
	val max = 16
	val data = alloc (max, 0.0)
        fun init (i) =
	    if i < max then
		let
		    val _ = update (data, i, float_of_int ((16 - i) * i))
		in
		    init (i+1)
		end
	    else ()
#	withtype {i:nat} int(i) -> unit
	val _ = init (0)
    in
	heapsort (H (max, 0, data))
    end
#withtype <> => unit -> unit
