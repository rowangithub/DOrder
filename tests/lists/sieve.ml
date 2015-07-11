(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)
let rec interval min max =
  if min > max then [] 
	else min :: interval (min+1) max
(*withtype {min:int} int (min) ->
         {max:int | min <= max+1} <max-min+1> =>
         int(max) -> int list(max-min+1)*)

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)
let rec filter p xs = 
	match xs with
		| [] -> []
		| (x::xs) -> 
			if (p x > 0) then x :: (filter p xs) 
			else filter p xs
(*withtype ('a -> bool) ->
         {n:nat} <n> => 'a list(n) -> [n':nat | n' <= n] 'a list(n')*)



(* The sieve itself *)
let rec sieve max lst =
  match lst with
    [] -> []
  | x :: xs ->
    if (x ) > max then lst
    else x :: (sieve max (filter ((fun m -> if (m > 0) then 1 else 0)) xs))
(*withtype {max:int | max >= 2} int(max) ->
         {l:nat} <l> => int list(l) -> [l':nat | l' <= l] int list(l')*)

let main max = 
	let l = interval 2 max in
	let ll = sieve max l in 
	assert (List.length ll <= List.length l)
(*withtype {max:int | max >= 2} <> => int(max) -> int list*)

let _ = main 5