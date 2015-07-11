val arraysize: ('a).{n:nat} 'a array(n) -> int(n)

fun bs_aux vec key l u =
  if u + 1 = l then ~1
  else
    let
        val m = l + (u-l) / 2
        val x = sub (vec, m)
    in
      	if x < key then bs_aux vec key (m+1) u
        else if x > key then bs_aux vec key l (m-1)
             else m
    end
withtype {n:nat} int array(n) -> int ->
         {i:int,j:int | 0 <= i <= j+1 <= n} <j+1-i> =>
         int(i) -> int(j) -> int

fun bs vec key = bs_aux vec key 0 (arraysize vec - 1)
withtype {n:nat} <> => int array(n) -> int -> int
