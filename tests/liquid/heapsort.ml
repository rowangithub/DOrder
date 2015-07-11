let rec heapify size data i =
	let left = 2 * i in
	let right = 2 * i in
  let left = left + 1 in
  let right = right + 2 in
	let largest =
	    (if left < size then
        let gl = Array.get data left in
        let gi = Array.get data i in
		 if gl > gi then left else i
	     else i (*: int[0,n)*))
  in
	let largest =
	    (if right < size then
		 if Array.get data right > Array.get data largest then right
		 else largest
	     else largest(*: int[0,n)*))
  in
	if largest > i then
		let temp = Array.get data i in
    let temp2 = Array.get data largest in
		let _none = Array.set data i temp2 in
		let _none = Array.set data largest temp in
	    heapify size data largest
	else ()
(*withtype {n:nat,i:nat | i < n} <n-i> =>
         float heap(n) * int(i) -> unit*)

let buildheap size data =
	let rec loop i =
	    if i >= 0 then
		    let _none = heapify size data i in 
        let i' = i - 1 in
        loop i' 
	    else ()
(*withtype {i:int | i < n} <max(i+1,0)> => int(i) -> unit*)
    in
    let size' = size / 2 in
    let size'_1 = size' - 1 in
	  loop size'_1
(*withtype {n:nat} float heap(n) -> unit*)

let heapsort maxx size data =
	let  _none = buildheap size data in
	let rec loop i =
	    if i >= 1 then
		    let temp = Array.get data i in
        let gd0 = Array.get data 0 in
		    let _none = Array.set data i gd0 in
		    let _none = Array.set data 0 temp in
		    let _none = heapify i data 0 in
        let i' = i - 1 in
		    loop i'
	    else ()
        (*withtype {i:int | i < n} <max(i,0)> => int(i) -> unit*)
    in
    let maxx' = maxx - 1 in
	loop maxx'
(*withtype {n:nat} float heap(n) -> unit*)

let print_array	data i j =
	let rec loop k =
	  if k < j then
      let sdk = Array.get data k in
 	    (*let _none = print_float sdk in*)
      let k' = k + 1 in
      loop k'
    else ()
        (*withtype {k:int | i <= k <= j} <j-k> => int(k) -> unit*)
  in
	loop i
(*withtype {n:nat, i:int, j:int | 0 <= i <= j <= n} <j-i> =>
         float array(n) * int(i) * int(j) -> unit*)

let main _none =
	let maxx = 16 in
	let data = Array.make maxx 0 in
    let rec init i =
	    if i < maxx then
        let isq = i * i in
        let i_16 = 16 * i in 
        let diff = i_16 - isq in
		    let _none = Array.set data i diff in
        let i' = i + 1 in
		    init i'
	    else ()
	(*withtype {i:nat} int(i) -> unit*)
  in
	let _none = init 0 in
	heapsort maxx 0 data; print_array data 0 maxx; data
(*withtype <> => unit -> unit*)

                                                  
let driver = main ()


(*(*datatype 'a heap with nat =
  {n:nat} H(n) of int(n) * [a:nat | a <= n] int(a) * 'a array(n)
                  (* maximum size, current size, data *)*)

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
(*withtype {n:nat,i:nat | i < n} <n-i> =>
         float heap(n) * int(i) -> unit*)


fun buildheap (h) =
    let
	val H(_, size, data) = h
	fun loop (i) =
	    if i >= 0 then
		let val _ = heapify (h, i) in loop (i - 1) end
	    else ()
(*withtype {i:int | i < n} <max(i+1,0)> => int(i) -> unit*)
    in
	loop (size / 2 - 1)
    end	
(*withtype {n:nat} float heap(n) -> unit*)


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
        (*withtype {i:int | i < n} <max(i,0)> => int(i) -> unit*)
    in
	loop (max - 1)
    end
(*withtype {n:nat} float heap(n) -> unit*)

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
        (*withtype {k:int | i <= k <= j} <j-k> => int(k) -> unit*)
    in
	loop (i)
    end			
(*withtype {n:nat, i:int, j:int | 0 <= i <= j <= n} <j-i> =>
         float array(n) * int(i) * int(j) -> unit*)


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
	(*withtype {i:nat} int(i) -> unit*)
	val _ = init (0)
    in
	heapsort (H (max, 0, data))
    end
(*withtype <> => unit -> unit*)*)
