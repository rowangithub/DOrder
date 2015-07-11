(*
 * 'a list(n) is the type for lists of length n
 * [] is assigned the type 'a list(0)
 * op:: is assigned the type {n:nat} 'a * 'a list(n) -> 'a list(n+1)
 *)
fun f [] k = k []
  | f (c :: cs) k = (* note that #"c" represents a constant character c *)
    if char_eq (c, #"(") then f cs (fn cs' => (f cs' k) + 1)
    else if char_eq (c, #")") then k (cs) - 1 else ~1
withtype {n:nat} <n> => char list(n) -> ({n':nat | n' <= n} char list(n') -> int) -> int
