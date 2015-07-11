let bsearch key vec =
  let rec look lo hi =
    let hi_minus = hi - 1 in
      if lo < hi_minus  then
        let hl = hi + lo in
        let m = hl in
        let x = Array.get vec m in
        let diff = key - x in
        let m_plus = m + 1 in
        let m_minus = m - 1 in
          (if diff < 0 then look lo m_minus
           else if 0 < diff then look m_plus hi
           else if key = x then m else -1)
      else -1
  in
  let sv = Array.length vec in
  let sv_minus = sv - 1 in 
    look 0 sv_minus

let driver =
  let _none = Random.init 555 in
  let sz = Random.int 10 in
  let sz_plus = sz + 2 in
  let ar = Array.make sz_plus 0 in
    bsearch 5 ar



(*qualif LE(x) = x < size vec;;

let bsearch key vec =
  let rec look lo hi =
    if lo < hi - 1  then
   let hl = hi + lo in
      let m = hl (*/ 2*)  in
      let x = lo + vec (*get vec m*) in
   let diff = key - x in
        (if diff < 0 then look lo (m - 1) 
    else if 0 < diff then look (m + 1) hi
    else if key = x then m else -1)
   else -1
 in
 let sv = 4 (*size vec*) in
 let sv_minus = sv - 1 in
 look 0 sv_minus 
in 
let ar = 2(*[|1;2;3|]*) in
bsearch 5 ar
;;*)


(* doesn't turn into a ptop_eval until the second argument is defined... *)


(*let{size:nat}
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
  withtype {l:int}{h:int | 0 <= l <= size /\ 0 <= h+1 <= size }
           int(l) * int(h) -> 'a answer
  in look (0, vect_length vec - 1)
withtype ('a -> 'a -> order) -> 'a -> 'a vect(size) -> 'a answer
;;

let bs key vec =
  let cmp i j =
    let res = compare i j in
      if res < 0 then LESS
      else if res = 0 then EQUAL
           else GREATER
  in bsearch cmp key vec
;;*)
