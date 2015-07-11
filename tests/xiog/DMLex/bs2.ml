val arraysize: ('a).{n:nat} 'a array(n) -> int(n)

fun bs vec key =
    let
	fun bs_aux l u =
	    if u + 1 = l then ~1
	    else
		let
		    val m = l + (u-l) / 2
		    val x = sub (vec, m)
		in
		    if x < key then bs_aux (m+1) u
		    else if x > key then bs_aux l (m-1)
			 else m
		end
#	withtype {i:int,j:int | 0 <= i <= j+1 <= n}  =>
#                 int(i) -> int(j) -> int
    in
	bs_aux 0 (arraysize vec - 1)
    end
#withtype {n:nat} <> => int array(n) -> int -> int
