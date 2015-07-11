(*datatype 'a Tree with nat =
  E(0) | {sl:nat, sr:nat} T(1+sl+sr) of 'a * 'a Tree(sl) * 'a Tree(sr)*)
type 'a tree = E | T of 'a * 'a tree * 'a tree

(*datatype 'a TreeList with (nat, nat) =
  Tnil(0,0) | {n:nat,s:nat,s':nat} Tcons(s+s',n+1) of 'a Tree(s) * 'a TreeList(s',n)*)
type 'a treelist = Tnil | Tcons of 'a tree * 'a treelist

let rec treesize t = match t with
	| E -> 0
	| T (v, t1, t2) -> 1 + treesize t1 + treesize t2

let rec treelistsize tl = match tl with
	| Tnil -> 0
	| Tcons (t, tl) -> treesize t + treelistsize tl

let rec refTree t = match t with
	| E -> E
	| T (x, a, b) -> T (x, refTree a, refTree b)
(*withtype {s:nat} 'a Tree(s) -> (int ref) Tree(s)*)

let rec intTree t = match t with
	| E -> E
  | T (r, a, b) -> T (r, intTree a, intTree b)
(*withtype {n:nat} <n> => (int ref) Tree (n) -> int Tree (n)*)

let rec aux t res =
	match t with
		| Tnil -> res
		| Tcons (t, ts) -> aux ts (Tcons (t, res))
(*withtype {n:nat,s:nat,n':nat,s':nat} <n> =>
'a TreeList(s,n) * 'a TreeList(s',n') -> 'a TreeList(s+s',n+n') *)

let revTreeList ts =
    let res = aux ts Tnil in
		(assert (treelistsize res = treelistsize ts); res)
(*withtype {n:nat,s:nat} <> => 'a TreeList (s,n) -> 'a TreeList (s,n)*)


let rec bfLab i t1 t2 = 
	match t1 with
		| Tnil -> (match t2 with
			| Tnil -> ()
			| Tcons (t, tl) -> bfLab i (revTreeList t2) Tnil 
			)
		| Tcons (t, tl) -> (match t with
			| E -> bfLab i tl t2
			| T (r, a, b) -> bfLab (i+1) tl (Tcons (b, Tcons (a, t2)))
			)	
(*withtype {n:nat,s:nat,n':nat,s':nat} <s+s',n+n',n'> =>
         int -> (int ref) TreeList(s,n) -> (int ref) TreeList(s',n') -> unit*)


let bfnum t =
  let t = refTree t in
  let _ = bfLab 1 (Tcons(t,Tnil)) Tnil in
  let t' = intTree t in
	(assert (treesize t = treesize t'); t')
(*withtype {n:nat} 'a Tree (n) -> int Tree (n)*)

(*The program does not provide construction method*)
let main () = 
	bfnum (T (1, T (2, E, E), T (3, E, E)))
let _ = main ()