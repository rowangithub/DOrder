fun('a) bcopy_aux (src, des, i, m) =
  if i = m then ()
  else let
           val _ = update(des, i, sub(src, i))
       in
           bcopy_aux (src, des, i+1, m)
       end
#withtype {m:nat, n:nat, i:nat | i <= m <= n}  =>
#         'a array(m) * 'a array(n) * int (i) * int(m) -> unit

fun('a) bcopy(src, des) = bcopy_aux (src, des, 0, arraysize src)
#withtype {m:nat, n:nat | m <= n}  => 'a array(m) * 'a array(n) -> unit
