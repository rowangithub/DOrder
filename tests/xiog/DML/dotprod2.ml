let dotprod v1 v2 = begin
  let sum = ref 0 in
    for i = 0 to pred (vect_length v1) do
      sum := v1..(i) * v2..(i) + !sum
    done;
    !sum
end 
#                     withtype {n:nat} int vect(n) -> int vect(n) -> int
;;
