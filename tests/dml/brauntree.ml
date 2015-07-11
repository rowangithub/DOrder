(* An implementation of Braun trees in DML *)
type 'a brauntree =
	| L
  | B of 'a * 'a brauntree(*m*) * 'a brauntree(*n*) (*{m:nat, n:nat | n <= m <= n+1}*)

(*The measure functions of brauntree*)
let rec volumn t = match t with
	| L -> 0
	| B (x, t1, t2) -> 1 + volumn t1 + volumn t2

let volumn1 t = match t with
	| L -> 0
	| B (x, t1, t2) -> volumn t1

let volumn2 t = match t with
	| L -> 0
	| B (x, t1, t2) -> volumn t2

let rec diff k t = 
	let _ = assert (k >= 0) in
	let res = match t with
  | L -> 0
  | B(x, l, n) ->
    if k = 0 then 1
    else if k mod 2 = 1 then diff (k/2) l 
		else diff ((k/2) - 1) n in
	res
(*We want to infer k <= volumn(t) <= k+1 and
			volumn (t) - k = diff k t*)

(*HACK: the parameter a ensures `size` is not considered 
 as a measure function *)
let rec size a t = 
	let res = match t with
  | L -> 0
  | B(x, l, n) ->
    let rn = size a n in 
		1 + rn + rn + (diff rn l) in
	(assert (res = volumn t); res)
(*We want to prove that size (t) = volumn (t)*)

let rec insert x t = 
	let res = match t with
	| L -> B (x, L, L)
	| B (e, l, n) ->
		if  x < e then B (x, insert e n, l)
    else B (e, insert x n, l)	in
	(assert (size (-1) res = volumn t + 1); res)
(* We want to prove that the braun tree size is incremented by 1*)
