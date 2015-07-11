fun('a) bcopy_aux (src, des, i, l, j) =
  if i = l then ()
  else let
           val _ = update(des, j, sub(src, i))
       in
           bcopy_aux (src, des, i+1, l, j+1)
       end
withtype {m:nat,n:nat,i:nat,j:nat,l:nat | i <= l <= m, j+l <= i+n} <m-i> =>
         'a array(m) * 'a array(n) * int (i) * int(l) * int(j) -> unit

fun('a) bcopy (src, des, ss, len, ds) =
  if (ss + len <= arraysize src) then
    if (ds + len <= arraysize des) then
      bcopy_aux (src, des, ss, ss + len, ds)
    else ()
  else () 
withtype {m:nat,n:nat,ss:nat,len:nat,ds:nat} <> =>
         'a array(m) * 'a array(n) * int(ss) * int (len) * int(ds) -> unit
