type order = LESS | EQUAL | GREATER
and 'a answer = NotFound | Found of int
;;

let{size:nat}
bsearch cmp key vec =
  let rec look(lo, hi) =
    if ge_int hi lo then
      let m = (hi + lo) / 2 in
      let x = vec..(m) in
        match cmp key x with
          LESS -> look(lo, m-1)
        | GREATER -> look(m+1, hi)
        | EQUAL -> Found(m)
    else NotFound
#  withtype {l:int}{h:int | 0 <= l <= size /\ 0 <= h+1 <= size }
#           int(l) * int(h) -> 'a answer
  in look (0, vect_length vec - 1)
#withtype ('a -> 'a -> order) -> 'a -> 'a vect(size) -> 'a answer
;;

let bs key vec =
  let cmp i j =
    let res = compare i j in
      if res < 0 then LESS
      else if res = 0 then EQUAL
           else GREATER
  in bsearch cmp key vec
;;
