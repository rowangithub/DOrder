(*
datatype term = Var of string | Fun of string * term list

fun size (Var _) = 1
  | size (Fun (_, ts)) = 1 + sizeList (ts)
withtype <term> -> int

and sizeList (nil) = 0
  | sizeList (cons(t, ts)) = size (t) + sizeList (ts)
*)

datatype term with pos =
  Var(1) of string | {n:nat} Fun(n+1) of string * termList(n)

and termList = tnil(0) | {m:pos,n:nat} tcons(m+n) of term(m) * termList(n)

fun size (Var _) = 1
  | size (Fun (_, ts)) = 1 + sizeList (ts)
withtype {n:nat} <n,0> => term(n) -> int(n)

and sizeList (tnil) = 0
  | sizeList (tcons(t, ts)) = size (t) + sizeList (ts)
withtype {n:nat} <n,1> => termList(n) -> int(n)

datatype term with nat =
  Var(1) of string | {n:nat} Fun(n+1) of string * termList(n)

and termList with nat =
  tnil(0) | {m:nat,n:nat} tcons(m+n+1) of term(m) * termList(n)

fun size (Var _) = 1
  | size (Fun (_, ts)) = 1 + sizeList (ts)
withtype {n:nat} <n> => term(n) -> int

and sizeList (tnil) = 0
  | sizeList (tcons(t, ts)) = size (t) + sizeList (ts)
withtype {n:nat} <n> => termList(n) -> int