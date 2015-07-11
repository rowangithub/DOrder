datatype 'a sparseArray with (nat, nat) =
  {m:nat,n:nat} SA(m,n) of int(m) * int(n) * ((int[0,n) * 'a) list) array(m)

fun list_vec_mult (xs, vec) =
   let
       fun loop (xs, sum) =
           case xs of
             [] => sum
	   | (i, f) :: xs => loop (xs, sum +. f *. sub (vec, i))
       withtype {l:nat} <l> =>  (int[0,n) * float) list(l) * float -> float
   in
       loop (xs, 0.0)
   end
withtype {n:nat,l:nat} (int[0,n) * float) list(l) * float array(n) -> float

fun mat_vec_mult (mat, vec) =
    let
        val SA (row, _, data) = mat
        val res = alloc (row, 0.0)
        fun loop (i) =
            if i < row then
               let
		   val sum = list_vec_mult (sub (data, i), vec)
		   val _ = update (res, i, sum)
	       in
	           loop (i+1)
	       end
	    else ()
        withtype {i:nat | i <= m} <m-i> => int(i) -> unit
        val _ = loop (0)
    in
        res
    end
withtype {m:nat,n:nat} <> =>
         float sparseArray(m,n) * float array(n) -> float array(m)
