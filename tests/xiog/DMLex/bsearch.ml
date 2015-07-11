val arraysize: ('a).{n:nat} 'a array(n) -> int(n)

fun bs_aux key vec l u =
  if u < l then NONE
  else
    let
        val m = l + (u-l) / 2
        val x = sub (vec, m)
    in
      	if x < key then bs_aux key vec (m+1) u
        else if x > key then bs_aux key vec l (m-1)
             else SOME (m)
    end
withtype int -> {n:nat} int array(n) ->
         {i:int,j:int | 0 <= i <= j+1 <= n} <j+1-i> =>
         int(i) -> int(j) -> int option

fun bsearch (key, vec) = bs_aux key vec 0 (arraysize vec - 1)
withtype {n:nat} int * int array(n) -> int option
