datatype 'a matrix with (nat, nat) =
  {m:nat, n:nat}
  Matrix(m, n) of int(m) * int(n) * ('a array(n)) array(m)

fun('a) sub2 (data, i, j) = sub (sub (data, i), j)
#withtype {m:nat, n:nat, i:nat, j:nat | i < m, j < n}  =>
#         ('a array(n)) array(m) * int(i) * int(j) -> 'a

fun('a) update2 (data, i, j, x) = update (sub (data, i), j, x)
#withtype {m:nat, n:nat, i:nat, j:nat | i < m, j < n}  =>
#        ('a array(n)) array(m) * int(i) * int(j) * 'a -> unit

fun matmul (a, b) =
    let
	val Matrix (p, q, adata) = a
	val Matrix (_, r, bdata) = b
	val cdata = alloc (p, alloc (r, 0.0))

	fun init (i) =
            if i < p then
	       let
		   val _ = update (cdata, i, alloc (r, 0.0))
	       in
		   init (i+1)
	       end
	    else ()
#	withtype {i:pos}  => int(i) -> unit

	val _ = init (1)

	fun loop1 (i) =
	    if i < p then
		let
		    fun loop2 (j) =
			if j < r then
			    let
				fun loop3 (k, sum) =
				    if k < q then
					loop3 (k+1, sum +. sub2 (adata, i, k) *. sub2 (bdata, k, j))
				    else sum
#				withtype {k:nat}  => int(k) * float -> float
				val _ = update2 (cdata, i, j, loop3 (0, 0.0))
			    in
				loop2 (j+1)
			    end
			else ()
#		    withtype {j:nat}  => int(j) -> unit
		    val _ = loop2 (0)
		in
		    loop1 (i+1)
		end
	    else ()
#        withtype {i:nat}  => int(i) -> unit
	val _ = loop1 (0)
    in
	Matrix (p, r, cdata)
    end
#withtype {p:nat,q:nat,r:nat}  =>
#         float matrix(p,q) * float matrix(q,r) -> float matrix(p,r)
