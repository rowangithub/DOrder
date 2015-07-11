
fun fabs (f) =
  if (f >. 0.0) then f else (0.0 -. f)
#withtype <> => float -> float

fun('a) rowSwap (data, i, j) =
    let
	val temp = sub (data, i)
	val _ = update (data, i, sub (data, j))
    in
	update (data, j, temp)
    end
#withtype {m:nat,n:nat,i:nat,j:nat | i < m, j < m}  =>
#         ('a array(n)) array(m) * int(i) * int(j) -> unit

fun norm (r, n, i) =
    let
	val x = sub (r, i)
	val _ = update (r, i, 1.0)
	fun loop (k) =
	    if k < n then
		let
		    val _ = update (r, k, sub (r, k) /. x)
		in
		    loop (k+1)
		end
	    else ()
#	withtype {k:nat | k <= n}  => int(k) -> unit
    in
	loop (i+1)
    end
#withtype {n:nat, i:nat | i < n}  =>
#         float array(n) * int(n) * int(i) -> unit

fun rowElim (r, s, n, i) =
    let
	val x = sub (s, i)
	val _ = update (s, i, 0.0)
	fun loop (k) =
	    if k < n then
		let
		    val _ = update (s, k, sub (s, k) -. x *. sub (r, k))
		in
		    loop (k+1)
		end
	    else ()
#	withtype {k:nat | k <= n}  => int(k) -> unit
    in
	loop (i+1)
    end		
#withtype {n:nat, i:nat | i < n}  =>
#         float array(n) * float array(n) * int(n) * int(i) -> unit

fun rowMax (data, m, i) =
    let
	val x = fabs (sub2 (data, i, i))
	fun loop (j, x, max) =
            if j < m then
		let
		    val y = fabs (sub2 (data, j, i))
		in
		    if (y >. x) then loop (j+1, y, j)
		    else loop (j+1, x, max)
		end
	    else max
#        withtype {j:nat, max:nat | max < j <= m}  =>
#                 int(j) * float * int(max) -> [a:nat | a < m] int(a)
    in
	loop (i+1, x, i)
    end
#withtype {m:nat,n:nat,i:nat | i < m, i < n}  =>
#         (float array(n)) array(m) * int(m) * int(i) -> [a:nat | a < m] int(a)

datatype 'a matrix with (nat, nat) =
  {m:nat, n:nat}
  Matrix(m, n) of int(m) * int(n) * ('a array(n)) array(m)

fun gauss (mat) =
  let
      val Matrix (n, _, data) = mat
      fun loop1 (i) =
	  if i < n then
	      let
		  val max = rowMax(data, n, i)
		  val _ = norm (sub (data, max), n+1, i)
		  val _ = rowSwap (data, i, max)
		  fun loop2 (j) =
		      if j < n then
			  let
			      val _ = rowElim (sub (data, i),
					       sub (data, j), n+1, i)
			  in
			      loop2 (j+1)
			  end
		      else ()
#                  withtype {j:nat | j <= n}  => int(j) -> unit
		  val _ = loop2 (i+1)
	      in
		  loop1 (i+1)
	      end
	  else ()
#      withtype {i:nat | i <= n}  => int(i) -> unit
  in
      loop1 (0)
  end
#withtype {n:pos} <> => float matrix(n,n+1) -> unit

fun print_array	(data, i, j) =
    let
	fun loop (k) =
	    if k < j then
		let
		    val _ = print_string ("\t")
		    val _ = print_float (sub (data, k))
		in
		    loop (k+1)
		end
	    else print_string "\n"
#        withtype {k:int | i < k <= j}  => int(k) -> unit
    in
	if i < j then
	    let
		val _ = print_float (sub (data, i))
	    in
		loop (i+1)
	    end
	else print_string "\n"
    end			
#withtype {n:nat, i:int, j:int | 0 <= i <= j <= n}  =>
#         float array(n) * int(i) * int(j) -> unit

fun print_matrix (mat) =
    let
	val Matrix (m, n, data) = mat
	fun loop (i) =
	    if i < m then
		let
		    val _ = print_array (sub (data, i), 0, n)
		in
                    loop (i+1)
                end
	    else print_string ("\n")
#        withtype {i:nat | i <= m}  => int(i) -> unit
    in
	loop (0)
    end
#withtype {m:nat,n:nat} float matrix(m,n) -> unit


fun main () =
    let
	val data = alloc(3, alloc(4, 0.0))

	val _ = update2 (data, 0, 0, 1.0)
	val _ = update2 (data, 0, 1, 1.0)
	val _ = update2 (data, 0, 2, 1.0)
	val _ = update2 (data, 0, 3, 0.0 -. 4.0)

	val _ = update (data, 1, alloc (4, 0.0))
	val _ = update2 (data, 1, 0, 0.0 -. 2.0)
	val _ = update2 (data, 1, 1, 3.0)
	val _ = update2 (data, 1, 2, 1.0)
	val _ = update2 (data, 1, 3, 7.0)

	val _ = update (data, 2, alloc (4, 0.0))
	val _ = update2 (data, 2, 0, 3.0)
	val _ = update2 (data, 2, 1, 0.0 -. 1.0)
	val _ = update2 (data, 2, 2, 2.0)
	val _ = update2 (data, 2, 3, 7.0)

	val mat = Matrix (3, 4, data)

	val _ = print_matrix (mat)
	val _ = gauss (mat)
	val _ = print_matrix (mat)
    in
	()
    end
#withtype unit -> unit
