let
sortRange arr start n =
  let item _1_i = Array.get arr _1_i in
  let swap i j =
    let tmpj = item j in
    let tmpi = item i in Array.set arr i tmpj; Array.set arr j tmpi
  in
  let rec vecswap _3_i _2_j _1_n = 
		let _3_i_plus = _3_i + 1 in
		let _2_j_plus = _2_j + 1 in
		let _1_n_minus = _1_n - 1 in
		if _1_n = 0 then () else begin swap _3_i _2_j; vecswap _3_i_plus _2_j_plus _1_n_minus end
  in

  let insertSort start n =
    let limit = start + n in
		let start_plus = start + 1 in
    let rec outer i  =
			let i_plus = i + 1 in
      if limit < i_plus then ()
      else let rec inner j =
						 let j_minus = j - 1 in
             if j < start_plus then outer i_plus else 
								let ij_minus = item j_minus in 
									let ij = item j in  
									 if ij < ij_minus then (swap j j_minus; inner j_minus) else (outer i_plus)  
		       in inner i  
    in outer start_plus 
  in insertSort start n
(*let sorting _2_arr = (sortRange _2_arr 0 (Array.length _2_arr) ; _2_arr) in*)

let
sorted _1_arr =
  let len = Array.length _1_arr in
  let rec s v _5_i =
		let _5_i_plus = _5_i + 1 in
    let v' = Array.get _1_arr _5_i in
			if v' < v then false else 
				(if _5_i_plus = len then true else s v' _5_i_plus)
  in
  if len < 2 then true else s (Array.get _1_arr 0) 1

let driver =
  let noners = Random.init 555 in
  let sz = Random.int 10 in
  let sz_plus = sz + 5 in
  let sz_minus = sz_plus - 1 in
  let yarr = Array.make sz_plus 0 in 
    sortRange yarr 0 sz_minus; sorted yarr

(*type order = LESS | EQUAL | GREATER
;;

let{Array.length:nat}
sortRange(arr, start, n, cmp) =
  let item i = arr..(i) withtype {i:nat | i < Array.length } int(i) -> 'a in
  let swap (i,j) =
    let tmp = item i in arr..(i) <- item j; arr..(j) <- tmp
  withtype {i:nat}{j:nat | i < Array.length /\ j < Array.length } int(i) * int(j) -> unit in
  let rec vecswap (i,j,n) = if eq_int n 0 then () else begin swap(i,j); vecswap(i+1,j+1,n-1) end
  withtype {i:nat}{j:nat}{n:nat | i+n <= Array.length /\ j+n <= Array.length } int(i) * int(j) * int(n) -> unit in

  let insertSort (start, n) =
    let limit = start+n in
    let rec outer i =
      if ge_int i limit then ()
      else let rec inner j =
             if le_int j start then outer(i+1)
             else let j' = j - 1 in
               match cmp(item j',item j) with
                 GREATER -> (swap(j,j'); inner j')
               | _  -> outer(i+1)
           withtype {j:nat | j < Array.length } int(j) -> unit in inner i
    withtype {i:nat} int(i) -> unit in outer(start+1)
  withtype {start:nat}{n:nat | start+n <= Array.length } int(start) * int(n) -> unit in
  insertSort (start, n)
withtype {start:nat}{n:nat | start+n <= Array.length }
         'a vect(Array.length) * int(start) * int(n) * ('a * 'a -> order) -> unit
;;

let sorting arr cmp = sortRange(arr, 0, vect_length arr, cmp); arr
withtype {Array.length:nat} 'a vect(Array.length) -> ('a * 'a -> order) -> 'a vect(Array.length)
;;

(* sorted checks if a list is well-sorted *)
let{Array.length:nat}
sorted cmp arr =
  let len = vect_length arr in
  let rec s (v,i) =
    let v' = arr..(i) in
      match cmp(v,v') with
        GREATER -> false
      | _ -> if eq_int (i+1) len then true else s(v',i+1)
  withtype {i:nat | i < Array.length } 'a * int(i) -> bool in
  if le_int len 1 then true else s(arr..(0),1)
withtype ('a * 'a -> order) -> 'a vect(Array.length) -> bool
;;*)
