(*
USED: PLDI2011 as a-cppr
USED: PEPM2013 as a-cppr
*)

let make_array n i = assert (0 <= i && i < n); 0
let update (i:int) (n:int) des x : int -> int =
  des i;
  let a j = if i=j then x else des j in a
let print_int (n:int) = ()
let f (m:int) src des =
  let rec bcopy (m:int) src des i =
    if i >= m then
      des
    else
      let des = update i m des (src i) in
      bcopy m src des (i+1)
  in
  let rec print_array m (array:int->int) i =
    if i >= m then
      ()
    else
      (print_int (array i);
       print_array m array (i + 1))
  in
  let array : int -> int = bcopy m src des 0 in
    print_array m array 0
let main n =
  let array1 = make_array n in
  let array2 = make_array n in
    if n > 0 then f n array1 array2
