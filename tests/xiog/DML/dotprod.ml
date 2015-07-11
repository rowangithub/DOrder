let
#{n:nat} 
  dotprod v1 v2 = begin
    loop (vect_length v1) 0 0
      where rec loop n sum i =
        if eq_int i n then sum else loop n (sum + (v1..(i) * v2..(i) : int)) (i+1)
#    withtype int(n) -> int -> {i:nat | i <= n} int (i) -> int
end 
#                             withtype int vect(n) -> int vect(n) -> int
;;

