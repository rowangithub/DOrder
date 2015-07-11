(*
fun f(x) = x +. x
withtype float -> float

fun ('a) f(x) = if x = 0 then 0 else x + f(x-1)
withtype {n:nat} <n> => int(n) -> int

fun ('a) g(x) = if x <= 0 then 0 else x + g(x-1)
withtype {n:int} <max(n,0)> => int(n) -> int
*)

fun('a,'b,'c) comp f g (x) = g (f (x))
withtype ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)

fun id (x) = x
withtype {n:nat} int(n) -> int(n)

val iid = (comp (id) (id): {n:nat} int(n) -> int(n))

