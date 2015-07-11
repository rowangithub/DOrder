fun bsearch key vec =
   let
	fun bs_aux l u =
	  if u < l then NONE
	  else
	      let
		  val m = l + (u-l) / 2
		  val x = sub (vec, m)
	      in
		  if x < key then bs_aux (m+1) u
		  else if x > key then bs_aux l (m-1)
		       else SOME (m)
	      end
	withtype {i:int,j:int | 0 <= i <= j+1 <= n} <j+1-i> =>
                 int(i) -> int(j) -> int option
   in
       bs_aux 0 (arraysize vec - 1)
   end
withtype <> => int -> {n:nat} int array(n) -> int option

