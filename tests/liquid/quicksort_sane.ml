let min m n = if m <= n then m else n 
(*
 * This is an example showing that array bounds checking
 * is not needed for doing quicksort on an array.
 * The code is copied from SML/NJ lib with some modification.
 *)
let rec sortRange arr start n =
  let item i = Array.get arr i in
  let swap i j =
    let tmp = item i in 
    (Array.set arr i (item j); Array.set arr j tmp)
  in
  let rec vecswap i j n = 
		if n = 0 then () else (swap i j; vecswap (i+1) (j+1) (n-1)) 
  in 

  (* calculate the median of three *)
  let med3 a b c =
    let a' = item a in
		let b' = item b in 
		let c' = item c in
		let lt_ab = a' < b' in
		let lt_bc = b' < c' in
		let gt_bc = c' < b' in
		let lt_ac = a' < c' in
		if lt_ab && lt_bc then
			b else
		if lt_ab then
			if lt_ac then c else a else
		if gt_bc then b else
		if lt_ac then a else c
	in 

  (* generate the pivot for splitting the elements *)
  let getPivot a n =
    if n <= 7 then a + n/2  
    else let p1 = a in
				 let pm = a + n/2 in
				 let pn = a + n - 1 in
       	 if n <= 40 then med3 p1 pm pn else
      		 let d = n / 8 in
					 let _2d = 2 * d in
					 let p1_plus_d = p1 + d in
					 let p1_plus_2_d = p1 + _2d in
					 let pm_minus_d = pm - d in
					 let pm_plus_d = pm + d in
					 let pn_minus_2_d = pn - _2d in
					 let pn_minus_d = pn - d in
           let newp1 = med3 p1 p1_plus_d p1_plus_2_d in
	   			 let newpm = med3 pm_minus_d pm pm_plus_d in
	   			 let newpn = med3 pn_minus_2_d pn_minus_d pn in 
							med3 newp1 newpm newpn
	  (* insertSort is called if there are less than 8 elements to be sorted *)
  in
  let rec insertSort start n =
    let limit = start + n in
    let rec outer i =
      if i >= limit then ()
      else let rec inner j =
             if j <= start then outer (i+1)
             else let j' = j - 1 in
									if item j' > item j then
										(swap j j'; inner j') else
										outer (i+1)
           in inner i
    in outer (start+1)

  and quickSort a n =
    let rec bottom limit pa pb = 
			let arg = (pa, pb) in
      if limit < pb then arg else
			if item a < item pb then arg else
				if item pb < item a then bottom limit pa (pb+1) else
						(swap pa pb; bottom limit pa(*(pa+1)*) (pb+1))
		in 
    let rec top limit pc pd = 
			let arg = (pc, pd) in
      if pc < limit then arg else
      if item pc < item a then arg else
			if item a < item pc then top limit (pc-1) pd else
			(swap pc pd; top limit (pc-1) pd(*(pd-1)*)) 
    in 
    let rec split pa pb pc pd =
			let (_, pb) = bottom pc pa pb in
      let (pc, _) = top pb pc pd in
      if pb >= pc then (pa, pb, pc, pd)
      else 
      (swap pb pc; 
								 split pa (pb+1) (pc-1) pd) 
 	  in 

    let pm = getPivot a n in
    let pa = a + 1 in
    let pd = a + n - 1 in
    let _ = swap a pm in
    let (_, pb, pc, _) = split pa pa pd pd in

    let pn = a + n in
		
    let r = min (pa-a) (pb-pa) in
    let _ = vecswap a (pb-r) r in

    (*let r = min (pd-pc) (pn-pd-1) in
    let _ = vecswap pb (pn-r) r in*)
    let n' = pb - pa in

    let _ = if n' > 1 then sorting a n' else () in
    (*let _none = if 1 < n' then if n' > 7 then quickSort a n' else insertSort a n' else () in*)
    (*let _ = (fun y -> y) a in
    let _ = (fun y -> y) n' in
    let _ = if 1 < n' then (*quickSort a n'*)sortRange arr a n' else () in*)
    let n' = pd - pc in
      
    let _ = if n' > 1 then sorting (pn-n') n' else () in ()
    (*let __11_none = if 1 < n'' then if n'' > 7 then quickSort pn_minus_n'' n'' else insertSort a n'' else () in ()*)
    (*let _ = (fun y -> y) pn_minus_n' in
    let _ = (fun y -> y) n' in
    let _ = if 1 < n' then (*quickSort pn_minus_n' n'*)sortRange arr
     pn_minus_n' n' else () in ()*)

  and sorting a n = if n < 7 then insertSort a n else quickSort a n in
  (*let sorting _3_start _11_n = if _11_n < 7 then insertSort _3_start _11_n else quickSort _3_start _11_n
  in*) 
  sorting start n 
    (*quickSort start n*)
  (*if n <= 7 then insertSort start n else quickSort start n*)
(*let qs _1_vec =
  sortRange _1_vec 0 (Array.length _1_vec) *)
(*withtype {size:nat} 'a vect(size) -> unit*)
(*in*) 

(* sorted checks if a list is well-sorted *)
let
sorted arr =
  let len = Array.length arr in
  let rec s v k =
    let v' = Array.get arr k  in
			if v > v' then false else if k+1 = len then true else s v' (k+1)
	in
		if len <= 1 then true else 
			s (Array.get arr 0) 1 

let gen_vec rr =
    let rec fill_arr i = 
        let len = Array.length rr in
        if i < len then 
          let fill _none = Random.int 1000 in
          let i' = i + 1 in
          Array.set rr i (fill ()); fill_arr i' 
        else ()
    in fill_arr 0

let driver =
  let _ = Random.self_init ()in
  let p = Random.int 20 + 2 in
  let vec = Array.make p 0 in
  let _ = gen_vec vec in
  (*let vec = [|1;5;7;3;68;3;56;9;0;3;21|] in
  let lent = 10 in*)
	  sortRange vec 0 p; sorted vec



(* (*
 * This is an example showing that array bounds checking
 * is not needed for doing quicksort on an array.
 * The code is copied from SML/NJ lib with some modification.
 *)

(* 16 type annotations, which occupy about 40 lines *)

type order = LESS | EQUAL | GREATER
;;

let min m n = if le_int m n then m else n
withtype {m:int} int(m) -> {n:int} int(n) -> int(min(m, n))
;;

let{size:nat}
sortRange(arr, start, n, cmp) =
  let item i = arr..(i) withtype {i:nat | i < size } int(i) -> 'a in
  let swap (i,j) =
    let tmp = item i in arr..(i) <- item j; arr..(j) <- tmp
  withtype {i:nat}{j:nat | i < size /\ j < size } int(i) * int(j) -> unit in
  let rec vecswap (i,j,n) = if eq_int n 0 then () else begin swap(i,j); vecswap(i+1,j+1,n-1) end
  withtype {i:nat}{j:nat}{n:nat | i+n <= size /\ j+n <= size } int(i) * int(j) * int(n) -> unit in

  (* insertSort is called if there are less than 8 elements to be sorted *)
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
           withtype {j:nat | j < size } int(j) -> unit in inner i
    withtype {i:nat} int(i) -> unit in outer(start+1)
  withtype {start:nat}{n:nat | start+n <= size } int(start) * int(n) -> unit in

  (* calculate the median of three *)
  let med3(a,b,c) =
    let a' = item a and b' = item b and c' = item c in
      match cmp(a', b'), cmp(b', c') with
        LESS, LESS -> b
      | LESS, _ ->
         begin match cmp(a', c') with LESS -> c | _ -> a end
      | _, GREATER -> b
      | _ -> begin match cmp(a', c') with LESS -> a | _ -> c end
    withtype {a:nat}{b:nat}{c:nat | a < size /\  b < size /\ c < size}
             int(a) * int(b) * int(c) -> [n:nat | n < size ] int(n) in

  (* generate the pivot for splitting the elements *)

  let getPivot (a,n) =
    if le_int n 7 then a + n / 2
    else let p1 = a and pm = a + n / 2 and pn = a + n - 1 in
      if le_int n 40 then med3(p1,pm,pn)
      else let d = n / 8 in
           let p1 = med3(p1,p1+d,p1+2*d) in
	   let pm = med3(pm-d,pm,pm+d) in
	   let pn = med3(pn-2*d,pn-d,pn) in med3(p1,pm,pn)
  withtype {a:nat}{n:nat | 1 < n /\ a + n <= size}
           int(a) * int(n) -> [p:nat | p < size] int(p) in
		    
  let rec quickSort ((a, n) as arg) =
    let rec bottom(limit, ((pa, pb) as arg)) = (* this was defined as a higher order function in SML/NJ library *)
      if gt_int pb limit then arg
      else match cmp(item pb,item a) with
             GREATER -> arg
           | LESS -> bottom(limit, (pa, pb+1))
           | _ -> begin swap arg; bottom(limit, (pa+1,pb+1)) end
    withtype {l:nat}{ppa:nat}{ppb:nat | l < size /\ ppa <= ppb <= l+1 }
             int(l) * (int(ppa) * int(ppb)) ->
             [pa:nat][pb:nat | ppa <= pa <= pb <= l+1] (int(pa) * int(pb))

    and top(limit, ((pc, pd) as arg)) = (* this was defined as a higher order function in SML/NJ library *)
      if gt_int limit pc then arg
      else match cmp(item pc,item a) with
             LESS -> arg
           | GREATER -> top(limit, (pc-1,pd))
           | _ -> begin swap arg; top(limit, (pc-1,pd-1)) end
    withtype {l:nat}{ppc:nat}{ppd:nat | 0 < l <= ppc+1 /\ ppc <= ppd < size }
              int(l) * (int(ppc) * int(ppd)) ->
              [pc:nat][pd:nat | l <= pc+1 /\ pc <= pd <= ppd] (int(pc) * int(pd)) in

    let rec split (pa,pb,pc,pd) =
      let (pa,pb) = bottom(pc, (pa,pb)) in
      let (pc,pd) = top(pb, (pc,pd)) in
        if ge_int pb pc then (pa,pb,pc,pd)
        else begin swap (pb,pc); split (pa,pb+1,pc-1,pd) end
    withtype {ppa:nat}{ppb:nat}{ppc:nat}{ppd:nat | 0 < ppa <= ppb <= ppc+1 /\ ppc <= ppd < size }
             int(ppa) * int(ppb) * int(ppc) * int(ppd) ->
             [pa:nat][pb:nat][pc:nat][pd:nat | ppa <= pa <= pb <= pc+1 /\ pc <= pd <= ppd]
             (int(pa) * int(pb) * int(pc) * int(pd)) in

    let pm = getPivot arg in
    let _ = swap(a,pm) in
    let pa = a + 1 
    and pc = a + n - 1 in
    let (pa,pb,pc,pd) = split(pa,pa,pc,pc)
    and pn = a + n in

    let r = min (pa - a) (pb - pa) in
    let _ = vecswap(a, pb - r, r) in
    let r = min (pd - pc) (pn - pd - 1) in
    let _ = vecswap(pb, pn - r, r) in
    let n' = pb - pa in
    let _ = if gt_int n' 1 then sorting(a,n') else () in
    let n' = pd - pc in
    let _ = if gt_int n' 1 then sorting(pn-n',n') else () in ()
  withtype {a:nat}{n:nat | 7 <= n /\ a+n <= size } int(a) * int(n) -> unit
		
  and sorting ((_, n) as arg) = if lt_int n 7 then insertSort arg else quickSort arg
  withtype {a:nat}{n:nat |  a+n <= size } int(a) * int(n) -> unit in
  sorting (start,n)
withtype {start:nat}{n:nat | start+n <= size }
         'a vect(size) * int(start) * int(n) * ('a * 'a -> order) -> unit
;;

let qs vec =
  let cmp (i, j) =
    let res = compare i j in
      if res < 0 then LESS
      else if res > 0 then GREATER
           else EQUAL
  in sortRange(vec, 0, (vect_length vec), cmp)
withtype {size:nat} 'a vect(size) -> unit
;;

(* sorted checks if a list is well-sorted *)
let{size:nat}
sorted cmp arr =
  let len = vect_length arr in
  let rec s(v,i) =
    let v' = arr..(i) in
      match cmp(v,v') with
        GREATER -> false
      | _ -> if eq_int (i+1) len then true else s(v',i+1)
  withtype {i:nat | i < size } 'a * int(i) -> bool in
  if le_int len 1 then true else s(arr..(0),1)
withtype ('a * 'a -> order) -> 'a vect(size) -> bool
;;

let vec = [|7;5;3;1;8;6;4;2|]
;;
*)
