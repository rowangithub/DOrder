(*
 * An example showing the necessity for imposing
 * some restrictions on datatype declaration
 *)

datatype omega = D of omega -> int
fun f(x) = case x of D g => g(x)
withtype <> => omega -> int

