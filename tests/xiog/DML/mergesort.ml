(* Merging and sorting *)

datatype 'a list =
  nil(0) | {n:nat} cons(n+1) of 'a * 'a list(n)
;;

datatype 'a listlist =
  listnil(0) | {m:nat}{n:nat} listcons(m+n) of 'a list(m) * 'a listlist(n)
;;

let rec merge order = fun
    nil l2 -> l2
  | l1 nil -> l1
  | (cons(h1, t1) as l1) (cons(h2, t2) as l2) ->
      if order h1 h2 then cons(h1, merge order t1 l2) else cons(h2, merge order l1 t2)
withtype ('a -> 'a -> bool) -> {m:nat}{n:nat} 'a list(m) -> 'a list(n) -> 'a list(m+n)
;;

(*
This style is not working yet:

let merge order =
  merge_rec where rec merge_rec = fun
    nil l2 -> l2
  | l1 nil -> l1
  | (cons(h1, t1) as l1) (cons(h2, t2) as l2) ->
      if order h1 h2 then cons(h1, merge_rec t1 l2) else cons(h2, merge_rec l1 t2)
  withtype {m:nat}{n:nat} 'a list(m) -> 'a list(n) -> 'a list(m+n)
;;
*)


let sorting order l =
  let rec initlist = function
      nil -> listnil
    | cons(_, nil) as singleton -> listcons(singleton, listnil)
    | cons(e1, cons(e2, rest)) -> (* Notice the type annotation below: a real lesson *)
        listcons((if order e1 e2 then cons(e1, cons(e2, nil)) else cons(e1, cons(e2, nil)) : 'a list(2)),
                 initlist rest)
  withtype {n:nat} 'a list(n) -> 'a listlist(n) in
  let rec merge2 = function
      listcons(l1, listcons(l2, rest)) -> listcons(merge order l1 l2, merge2 rest)
    | listcons(_, listnil) as x -> x
    | listnil as x -> x
  withtype {n:nat} 'a listlist(n) -> 'a listlist(n) in
  let rec mergeall = function
      listnil -> nil
    | listcons(l, listnil) -> l
    | listcons(_, listcons(_, _)) as llist -> mergeall (merge2 llist)
  withtype {n:nat} 'a listlist(n) -> 'a list(n) in
  mergeall(initlist l)
withtype ('a -> 'a -> bool) -> {n:nat} 'a list(n) -> 'a list(n)
;;
