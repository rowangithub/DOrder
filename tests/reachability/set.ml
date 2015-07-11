(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Sets over ordered types *)

type 'a tree = 
	| Empty
	| Node of 'a tree * 'a * 'a tree

(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

let rec height t = match t with
	| Empty -> 0
	| Node (t1, x, t2) -> 
		if (height t1 < height t2) 
		then 1 + height t2 
		else 1 + height t1
		
(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)	
	
(*let create y l n = 
	Node (l, y, n)	*)

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal y l n =
  let hl = height l in
 	let hr = height n in
  if hl > hr + 2 then
		match l with
			| Node (ll, lv, lr) ->
		    if height ll >= height lr then
		      Node (ll, lv, (Node (lr, y, n)))
		    else
		      match lr with
						| Node (lrl, lrv, lrr) ->
		          Node (Node (ll, lv, lrl), lrv, (Node (lrr, y, n)))
  else if hr > hl + 2 then
		match n with
			| Node (rl, rv, rr) ->
        if height rr >= height rl then
          Node (Node (l, y, rl), rv, rr)
        else
          match rl with
            Node (rll, rlv, rlr) ->
              Node (Node (l, y, rll), rlv, Node (rlr, rv, rr)) 
  else Node (l, y, n) 

(* Insertion of one element *)

let rec add x t =
	match t with
		| Empty -> Node (Empty, x, Empty) 
		| Node (l, x', n) ->
			if x' = x then Node (l, x, n)
			else if (x < x') then (*Node (add x l, x', n)*)
				bal x' (add x l) n
			else (*Node (l, x', add x n) *)
				bal x' l (add x n)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join y l n =
	match (l, n) with
		| (Empty, n) -> add y n
    | (l, Empty) -> add y l
    | (Node(ll, lv, lr), Node(rl, rv, rr)) ->
			let lh = height l in
			let rh = height n in
      if lh > rh + 2 then bal lv ll (join y lr n) else
      if rh > lh + 2 then bal rv (join y l rl) rr else
      Node (l, y, n)		(*create y l n*)
			
(* Smallest and greatest element of a set *)			
			
let rec min_elt (phantom:bool) t : int = 
	match t with
		(*| Empty -> raise Not_found*)
  	| Node(l, x, n) -> (
			match l with
				| Empty -> x
				| Node (l', x', n') -> min_elt phantom l
			)	
			
let rec max_elt (phantom:bool) t : int = 
	match t with
		(*| Empty -> raise Not_found *)
		| Node (l, x, n) -> (
			match n with
				| Empty -> x
				| Node (l', x', n') -> max_elt phantom n
			)
			
(* Remove the smallest element of the given set *)
						
let rec remove_min_binding t = 
	match t with
		(*| Empty -> invalid_arg "Map.remove_min_elt"*)
		| Node (l, x, n) -> (
			match l with
				| Empty -> (x, n)
				| Node (l', x', n') -> 
					let (m, l'') = remove_min_binding l in 
					(m, bal x l'' n(*Node (l'', x, n)*))
			) 

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge l n = 
	match (l, n) with
	| (Empty, t) -> t
	| (t, Empty) -> t
  | (t1, t2) -> 
		let (m, t2) = remove_min_binding t2 in
		bal m t1 t2

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat l n =
	match (l, n) with
		| (Empty, t) -> t
		| (t, Empty) -> t
    | (t1, t2) -> 
			let (m, t2) = remove_min_binding t2 in
			join m t1 t2
			
(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)			
			
let rec split z t =
	match t with
		| Empty ->
      (Empty, false, Empty)
    | Node(l, x, n) ->
			if (z = x) then
				(l, true, n)
      else if z < x then
        let (ll, pres, rl) = split z l in 
				(ll, pres, join x rl n)
      else
        let (lr, pres, rr) = split z n in 
				(join x l lr, pres, rr)
				
(* Implementation of the set operations *)

let emtpy = Empty

let is_empty t = 
	match t with 
		| Empty -> true
		| Node (l', x', n') -> false		

let rec mem x s = 
	match s with
		| Empty -> false
    | Node(l, y, n) ->
			if x = y then true
			else if x < y then
				mem x l
			else mem x n				
			
let singleton x = 
	Node (Empty, x, Empty)														
			
let rec remove x t = 
	match t with
		| Empty -> Empty
		| Node(l, x', n) ->
  		if x' = x then
    		merge l n
  		else if x' > x then
    		(*Node ((remove x l), x', n)*) bal x' (remove x l) n
  		else
    		(*Node (l, x', (remove x n))*) bal x' l (remove x n)	

let rec union s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1), Node(l2, v2, r2)) ->
		let h1 = height s1 in
		let h2 = height s2 in
		if h1 >= h2 then
    	(*if h2 = 1 then 
				add v2 s1 
			else*) (
      	let (l2, result, r2) = split v1 s2 in
      	join v1 (union l1 l2) (union r1 r2)
    	)
  	else
    	(*if h1 = 1 then 
				add v1 s2 
			else*) (
      	let (l1, result, r1) = split v2 s1 in
      	join v2 (union l1 l2) (union r1 r2)
    	)					
			
let rec inter s1 s2 =
	match (s1, s2) with
		| (Empty, t2) -> Empty
    | (t1, Empty) -> Empty
    | (Node(l1, v1, r1), t2) ->
			let (l2, result, r2) = split v1 t2 in
			if (result) then
				join v1 (inter l1 l2) (inter r1 r2)	
			else 
				concat (inter l1 l2) (inter r1 r2)
								
let rec diff s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1), t2) ->
		let (l2, result, r2) = split v1 t2 in
		if (result) then
			concat (diff l1 l2) (diff r1 r2)
		else
			join v1 (diff l1 l2) (diff r1 r2)		
			
let rec subset s1 s2 =
	match (s1, s2) with
		| Empty, s2 ->
      true
    | s1, Empty ->
      false
    | Node (l1, v1, r1), (Node (l2, v2, r2)) ->
          if v1 = v2 then
            subset l1 l2 && subset r1 r2
          else if v1 < v2 then
            subset (Node (l1, v1, Empty)) l2 && subset r1 s2
          else
            subset (Node (Empty, v1, r1)) r2 && subset l1 s2	
					
let rec iter f s = 
	match s with
		| Empty -> ()
    | Node(l, x, n) -> iter f l; f x; iter f n

let rec fold f s accu =
	match s with
		| Empty -> accu
    | Node(l, x, n) -> 
			fold f n (f x (fold f l accu))

let rec for_all p s =
	match s with
		| Empty -> true
    | Node(l, x, n) -> 
			p x && for_all p l && for_all p n

let rec exists p s =
	match s with
		| Empty -> false
    | Node(l, x, n) -> 
			p x || exists p l || exists p n

let rec filt accu p s =
	match s with 
	| Empty -> accu
	| Node(l, x, n) ->
    filt (filt (if p x then add x accu else accu) p l) p n 

let filter p s =
  filt Empty p s								
				
let rec part (t, f) p s = 
		match s with
    | Empty -> (t, f)
    | Node(l, x, n) ->
				let result = (part (if p x then (add x t, f) else (t, add x f)) p l) in
        part result p n		
					
let partition p s =
  part (Empty, Empty) p s		
	
let rec cardinal (phantom:bool) t = 
	match t with
		| Empty -> 0
		| Node (l, x, n) -> 
			cardinal phantom l + 1 + cardinal phantom n	
	
let rec elements_aux accu s = 
	match s with
		| Empty -> accu
    | Node(l, x, n) -> elements_aux (x :: elements_aux accu n) l

let elements s =
  elements_aux [] s																																								