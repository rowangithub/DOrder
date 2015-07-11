type 'a heap = 
	| E 
	| T of int * 'a * 'a heap * 'a heap 
 
let rank h = 
	match h with
		|  E -> 0 
		| T (rk,a,b,c) -> rk
 
let t x a b = 
  let a, b = 
		if rank a > rank b then a, b else b, a in 
  T (rank b + 1, x, a, b) 
 
let rec merge h1 h2 =
	match h1, h2 with 
  | h1, E -> h1 
	| E, h2 -> h2 
  | (T(rk1, x, a1, b1)), (T(rk2, y, a2, b2)) -> 
		if x >= y then 
			t x a1 (merge b1 h2) 
		else 
			t y a2 (merge h1 b2) 
 
let rec to_list h =
	match h with 
  | E -> [] 
  | T(rk, x, a, b) -> 
		x::(to_list (merge a b) )
		
let rec to_heap xs = 
	match xs with
		| [] -> E
		| x::xs' -> merge (t x E E) (to_heap xs')
 
let heapsort xs = 
	to_list (to_heap xs)