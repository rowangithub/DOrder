datatype 'a rlist with nat =
    Nil(0)
  | One(1) of 'a
  | {n:nat | n > 0} Even(n+n) of 'a rlist(n) * 'a rlist(n)
  | {n:nat | n > 0} Odd(n+n+1) of 'a * 'a rlist(n) * 'a rlist(n) ;;

exception List_is_empty ;;

let rec cons x = function
    Nil -> One x
  | One y ->  Even(One(x), One(y))
  | Even(l1, l2) -> Odd(x, l1, l2)
  | Odd(y, l1, l2) -> Even(cons x l1, cons y l2)
withtype {n:nat} 'a -> 'a rlist(n) -> 'a rlist(n+1) ;;

let rec uncons = function
    One x -> (x, Nil)
  | Even(l1, l2) ->
    let (x, l1) = uncons l1 and (y, l2) = uncons l2 in begin
      match l1 with
        Nil -> (x, One y)
      | One _ -> (x, Odd(y, l1, l2))
      | Even _ -> (x, Odd(y, l1, l2))
      | Odd _ -> (x, Odd(y, l1, l2))
    end
  | Odd(x, l1, l2) -> (x, Even(l1, l2))
withtype {n:nat | n > 0} 'a rlist(n) -> 'a * 'a rlist(n-1) ;;

let head_safe l =
 let (x, _) = uncons l in x
withtype {n:nat | n > 0} 'a rlist(n) -> 'a ;;

let tail_safe l =
  let (x, l) = uncons l in l
withtype {n:nat | n > 0} 'a rlist(n) -> 'a rlist(n-1) ;;

let head = function
    Nil -> raise List_is_empty
(*
  | l -> head_safe l
*)

  | One _ as l -> head_safe l
  | Even _ as l ->  head_safe l
  | Odd _ as l ->  head_safe l
withtype 'a rlist -> 'a ;;

let tail = function
    Nil -> raise List_is_empty
(*
  | l -> tail_safe l
*)
  | One _ as l -> tail_safe l
  | Even _ as l ->  tail_safe l
  | Odd _ as l ->  tail_safe l
withtype 'a rlist -> 'a rlist ;;

let rec length = function
    Nil -> 0
  | One _ -> 1
  | Even (l1, _) -> 2 * (length l1)
  | Odd (_, l1, _) -> 2 * (length l1) + 1
withtype {n:nat} 'a rlist(n) -> int(n) ;;

exception Subscript ;;

let rec lookup_safe i = function
    One x -> x
  | Even (l1, l2) ->
    if i mod 2 = 0 then lookup_safe (i / 2) l1 else lookup_safe (i / 2) l2
  | Odd(x, l1, l2) ->
    if eq_int i 0 then x
     else if eq_int (i mod 2) 0 then lookup_safe ((i - 1) / 2) l2
          else lookup_safe ((i - 1) / 2) l1
withtype {i:nat}{n:nat | i < n} int(i) -> 'a rlist(n) -> 'a ;;

(*
let rec lookup i l =
  let len = length l
  in if lt_int i 0 then raise Subscript
     else if ge_int i len then raise Subscript
          else lookup_safe i l
withtype int -> 'a rlist -> 'a ;;
*)

let rec lookup i = function
    Nil ->  raise List_is_empty
  | One x -> if i = 0 then x else raise List_is_empty
  | Even (l1, l2) ->
    if i mod 2 = 0 then lookup (i / 2) l1 else lookup (i / 2) l2
  | Odd(x, l1, l2) ->
    if i = 0 then x
     else if i mod 2 = 0 then lookup ((i - 1) / 2) l2
          else lookup ((i - 1) / 2) l1
withtype int -> 'a rlist -> 'a ;;

let rec print_rlist = function
    Nil -> ()
  | One _ as l -> let (x, _) = uncons l in print_int x; print_newline ()
  | Even _ as l -> let (x, l) = uncons l in print_int x; print_string "; "; print_rlist l
  | Odd _ as l -> let (x, l) = uncons l in print_int x; print_string "; "; print_rlist l ;;