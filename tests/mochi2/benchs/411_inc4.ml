let make_array (n:int) (i:int) = assert(0<=i && i<n); 0
let update (i:int) (n:int) (des:int->int) (x:int) = 
	des i; let a j = if i=j then x else des i in a
let rec inc3 (m:int) (a:int->int) (i:int) =
 if i>=m
 then ()
 else
   let a = update i m a ((a i)+1) in
     inc3 m a (i+1)
let main n i =
  if i=0 && n>0 then inc3 n (make_array n) i else ()
