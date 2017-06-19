let make_array n i = assert (0 <= i && i < n); 5

let update (i:int) (n:int) des (x:int) =
  let a (j:int) = if i=j then x else des j in a
	
let print_int (n:int) = ()

let f (m:int) src des =
  let rec bcopy (m:int) i (src: int->int) (des:int->int) =
    if i >= m then
      des
    else
      let des = update i m des (src i) in
      bcopy m (i+1) src des
  in
  let rec print_array m i (array:int->int) =
    if i >= m then
      ()
    else
      (print_int (array i);
       print_array m (i + 1) array)
  in
  let array : int -> int = bcopy m 0 src des in
    print_array m 0 array

let main n =
  let array1 = make_array n in
  let array2 = make_array n in
	if (n > 0) then
  	f n array1 array2
	else ()