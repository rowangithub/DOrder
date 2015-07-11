(* DSOLVE -dontgenmlq *)

(* Adapted from the Vec library by Luca de Alfaro
 * http://www.dealfaro.com/home/code/vec/vec.ml
 *)

type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

let empty = Empty

let rec height t = 
	match t with
	| Empty -> 0
	| Node (t1, v, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1
		
let height1 t = 
	match t with
	| Empty -> 0
	| Node (t1, v, t2) -> height t1

let height2 t = 
	match t with
	| Empty -> 0
	| Node (t1, v, t2) -> height t2		

let rec length t = 
	match t with
	| Empty -> 0
	| Node (t1, v, t2) -> 1 + length t1 + length t2 

let bal l x n =
  let hl = height l in
 	let hr = height n in
  if hl > hr + 2 then
		match l with
			| Node (ll, lv, lr) ->
		    if height ll >= height lr then
		      Node (ll, lv, (Node (lr, x, n)))
		    else
		      match lr with
						| Node (lrl, lrv, lrr) ->
		          Node (Node (ll, lv, lrl), lrv, (Node (lrr, x, n)))
  else if hr > hl + 2 then
		match n with
			| Node (rl, rv, rr) ->
        if height rr >= height rl then
          Node (Node (l, x, rl), rv, rr)
        else
          match rl with
            Node (rll, rlv, rlr) ->
              Node (Node (l, x, rll), rlv, Node (rlr, rv, rr)) 
  else Node (l, x, n) 

(* recbal as originally included in vec.ml did not provide a balancing guarantee. 
 * this has been fixed below by adding at worst a small constant number of rotations per level.
 * the fixed version also typechecks. *)
      
(* This is a recursive version of balance, which balances a tree all the way down. 
   The trees l and r can be of any height, but they need to be internally balanced.  
   Useful to implement concat. *)				
let rec recbal l d rn =
  let hl = height l in
  let hr = height rn in
    if hl > hr + 2 then begin
      match l with
        (*  Empty -> assert (1 = 0); assert false (*invalid_arg "Vec.bal"*)*)
        | Node(ll, ld, lr) ->
            if height ll >= height lr then
              bal ll ld (recbal lr d rn)
            else begin
              match lr with
                (*  Empty -> assert (1 = 0); assert false (*invalid_arg "Vec.bal"*)*)
                | Node(lrl, lrd, lrr) ->
                    let nr = recbal lrr d rn in
                      if height nr <= height lr - 3 then
                        Node (ll, ld, (bal lrl lrd nr))
                      else
                        Node ((Node (ll, ld, lrl)), lrd, nr)
            end
    end else if hr > hl + 2 then begin
      match rn with
        (*  Empty -> assert (1 = 0); assert false (*invalid_arg "Vec.bal"*)*)
        | Node(rl, rd, rr) ->
            if height rr >= height rl then
              bal (recbal l d rl) rd rr
            else begin
              match rl with
                (*  Empty -> assert (1 = 0); assert false (*invalid_arg "Vec.bal"*)*)
                | Node(rll, rld, rlr) ->
                    let nl = recbal l d rll in
                      if height nl <= height rl - 3 then
                        Node ((bal nl rld rlr), rd, rr)
                      else
                        Node (nl, rld, (Node (rlr, rd, rr)))
            end
    end 
    else Node (l, d, rn)				
					
let is_empty t = 
  match t with
    | Empty -> true
    | Node(l, d, r) -> false

let singleton d = Node (Empty, d, Empty)

let rec get i t =
  match t with
    (*  Empty -> assert (1 = 0); assert false (*raise Vec_index_out_of_bounds*)*)
    | Node (l, d, r) -> 
			let cl = length l in
	    if i < cl then get i l 
	    else if i > cl then get (i - cl - 1) r 
	    else d 
			
let rec set i d t =
  let t' = 
		match t with
    (*  Empty -> assert (1 = 0); assert false (*raise Vec_index_out_of_bounds*)*)
    | Node (l, dd, r) ->
			let cl = length l in 
      if i < cl then Node ((set i d l), dd, r)
      else if i > cl then Node (l, dd, (set (i - cl - 1) d r))
      else Node (l, d, r) in
	let _ = () in t' 
	
let rec remove_leftmost t = 
	match t with
	(*| Empty -> invalid_arg "Map.remove_min_elt"*)
	| Node (l, v, n) -> (match l with
		| Empty -> n
		| Node (l', v', n') -> bal (remove_leftmost l) v n 
		) 
			
let rec leftmost t = 
	match t with
	(*| Empty -> raise Not_found*)
  | Node(l, v, n) -> (match l with
		| Empty -> v
		| Node (l', v', n') -> leftmost l
		)		
		
let merge l m = 
	match (l, m) with
	| (Empty, t) -> t
	| (t, Empty) -> t
  | (t1, t2) -> 
		bal t1 (leftmost t2) (remove_leftmost t2)
			
let rec remove i t =
  match t with
    (*  Empty -> assert (1 = 0); assert false (*raise Vec_index_out_of_bounds*)*)
    | Node(l, d, n) ->
			  let cl = length l in
        if i < cl then 
	 				bal (remove i l) d n
        else if i > cl then 
	  			bal l d (remove (i - cl - 1) n)
        else merge l n
          
let rec insert i d t =
  let t' = 
		match t with
      Empty -> Node (Empty, d, Empty) (*begin
        if i = 0 
        then Node (Empty, 0, d, Empty, 0, 1)
        else (assert (1 = 0); assert false) (*raise Vec_index_out_of_bounds*)
      end*)
    | Node(l, dd, n) -> 
				let cl = length l in
        if i < cl then bal (insert i d l) dd n
        else if i > cl then bal l dd (insert (i - cl - 1) d n)
        else bal l d (insert 0 dd n) in
	let _ = assert (height t' >= height t && height t' <= height t + 1) in t'
	
let rec append d t =
  match t with
      Empty -> Node (Empty, d, Empty)
    | Node (l, dd, r) -> 
      bal l dd (append d r)	
 			
let rec pop i t =
  match t with
    (*  Empty -> assert (1 = 0); assert false (*raise Vec_index_out_of_bounds*)*)
    | Node(l, d, r) ->
			let cl = length l in
      if i < cl then 
	  		let (e, v) = pop i l in 
	    	(e, bal v d r)
      else if i > cl then 
	  		let (e, v) = pop (i - cl - 1) r in 
	    	(e, bal l d v)
      else (d, merge l r)					
	
let rec concat tr1 tr2 =
  match tr1 with
    | Empty -> tr2
    | Node(l, dd, n) -> (
        match tr2 with
          | Empty -> tr1
          | Node(l', dd', n') ->
              let d = leftmost tr2 in
              recbal tr1 d (remove_leftmost tr2)	
		)
		
let rec harness t1 t2 = 
	(concat t1 t2; harness t1 t2)
	
(*
let makenode x d y = 
	Node (x, d, y)
	
let rec create d n =
  if n = 0 then Empty 
	else
    let ml = n / 2 in 
    let mr = n - ml - 1 in 
    let l = create d ml in
    let rn = create d mr in (* defer this particular property to runtime *)
    makenode l d rn	

let rec sub i j t = 
	match t with 
		| Empty -> Empty
    | Node (l, dd, n) -> 
				let cl = length l in
				let cr = length n in
        if i >= j then Empty
	  		(* Important for sharing *)
        else if i <= 0 && j >= cl + cr + 1 then t
        else ( 
	  			if j <= cl then sub i j l 
	  			else if j = cl + 1 then append dd (sub i cl l)
	  			else if i = cl then insert 0 dd (sub 0 (j - cl - 1) n)
	  			else if i > cl then sub (i - cl - 1) (j - cl - 1) n
	  			else (
	    			(* dd straddles the interval *)
	    			let ll = sub i cl l in 
	    			let rr = sub 0 (j - cl - 1) n in 
	      		recbal ll dd rr
	  			)
        )	*)		
	
(*let rec iteri t f = 
  let rec offsetiteri t' (t: 'a tree) k =
    match t' with
        Empty -> ()
      | Node(l, d, r) ->
					let cl = length l in
          offsetiteri l t k;
          f (k + cl) d;
					assert (k+cl >= 0 && k+cl < length t);
          offsetiteri r t (k + cl + 1)
  in offsetiteri t t 0	
	
let rangeiteri i j t f  = 
  let rec offsetrangeiteri k (i:int) i' (j:int) j' t' = 
    match t' with
        Empty -> ()
      | Node(l, d, r) ->
					let cl = length l in
					let cr = length r in
          if i' < j' then begin 
            if i' < cl && j' > 0 then offsetrangeiteri k i i' j j' l else (); 
            if i' <= cl && j' > cl then f (k + cl) d else ();
            if j' > cl + 1 && i' <= cl + cr + 1 then offsetrangeiteri (k + cl + 1) i (i' - cl - 1) j (j' - cl - 1) r else ()
          end else ()
  in offsetrangeiteri 0 i i j j t 	
	
let revrangeiteri i j t f = 
  let rec offsetrevrangeiteri k i j t' =
    match t' with
        Empty -> ()
      | Node(l, d, r) ->
      let cl = length l in
			let cr = length r in
			if i < j then begin 
	    if j > cl + 1 && i <= cl + cr + 1 
	    then offsetrevrangeiteri (k + cl + 1) (i - cl - 1) (j - cl - 1) r else ();
	    if i <= cl && j > cl then f (k + cl) d else ();
	    if i < cl && j > 0 then offsetrevrangeiteri k i j l else ()
      end else ()
  in offsetrevrangeiteri 0 i j t 	
	
let mapi t f = 
  let rec offsetmapi k t' =
    match t' with
        Empty -> Empty
      | Node(l, d, r) ->
				let cl = length l in 
	  		Node(offsetmapi k l, f (k + cl) d, offsetmapi (k + cl + 1) r)
  in offsetmapi 0 t
       
let foldi t f accu =
  let rec offsetfoldi k t' accu = 
    match t' with
        Empty -> accu
      | Node(l, d, r) ->
				let cl = length l in
	  		offsetfoldi (k + cl + 1) r (f (k + cl) d (offsetfoldi k l accu))
  in offsetfoldi 0 t accu

let rangefoldi i j t f accu = 
  let rec offsetrangefoldi k i j t' accu = 
    match t' with 
      Empty -> accu
    | Node (l, d, r) -> 
	  if i >= j then accu
	  else begin 
			let cl = length l in
			let cr = length r in
	    let al = if i < cl && j > 0 then offsetrangefoldi k i j l accu else accu in 
	    let ad = if i <= cl && j > cl then f (cl + k) d al else al in 
	      if j > cl + 1 && i <= cl + cr + 1
	      then offsetrangefoldi (k + cl + 1) (i - cl - 1) (j - cl - 1) r ad
	      else ad
	  end
  in offsetrangefoldi 0 i j t accu 

let revfoldi t f accu =
  let rec offsetrevfoldi k t' accu = 
    match t' with
        Empty -> accu
      | Node(l, d, r) ->
				let cl = length l in
	  		offsetrevfoldi k l (f (k + cl) d (offsetrevfoldi (k + cl + 1) r accu))
  in offsetrevfoldi 0 t accu

let revrangefoldi i j t f accu = 
  let rec offsetrevrangefoldi k i j t' accu = 
    match t' with 
        Empty -> accu
      | Node (l, d, r) -> 
	  if i >= j then accu
	  else begin 
			let cl = length l in
			let cr = length r in
	    let ar = if j > cl + 1 && i <= cl + cr + 1
	    then offsetrevrangefoldi (k + cl + 1) (i - cl - 1) (j - cl - 1) r accu
	    else accu
	    in 
	    let ad = if i <= cl && j > cl then f (cl + k) d ar else ar in 
	      if i < cl && j > 0 then offsetrevrangefoldi k i j l ad else ad
	  end
  in offsetrevrangefoldi 0 i j t accu*) 