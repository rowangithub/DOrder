(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)

fun interval min max =
  if min > max then [] else min :: interval (min+1) max
withtype {min:int} int (min) ->
         {max:int | min <= max+1} <max-min+1> =>
         int(max) -> int list(max-min+1)

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

fun ('a)
  filter p [] = []
| filter p (a :: r) = if p a then a :: filter p r else filter p r
withtype ('a -> bool) ->
         {n:nat} <n> => 'a list(n) -> [n':nat | n' <= n] 'a list(n')

(* Application: removing all numbers multiple of n from a list of integers *)

fun remove_multiples_of n lst =
  filter ((fn m => m % n <> 0): int -> bool) lst
withtype <> => int -> {l:nat} int list(l) -> [l':nat | l' <= l] int list(l')

(* The sieve itself *)

fun sieve_aux max lst =
  case lst of
    [] => []
  | a :: r =>
    if (a * a: int) > max then lst
    else a :: sieve_aux max (remove_multiples_of a r)
withtype {max:int | max >= 2} int(max) ->
         {l:nat} <l> => int list(l) -> [l':nat | l' <= l] int list(l')

fun sieve max = sieve_aux max (interval 2 max)
withtype {max:int | max >= 2} <> => int(max) -> int list
