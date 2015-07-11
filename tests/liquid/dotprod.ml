let dotprod v1 v2 = 
		let rec loop n sum i =
			if i = n then sum else 
        loop n ((Array.get v1 i) * (Array.get v2 i) + sum) (i+1)
		in
    loop (Array.length v1) 0 0 

let driver =
  let _ = Random.init 555 in
  let sz = Random.int 40 in
  let v1 = Array.make sz 1 in
  let v2 = Array.make sz 1 in
    dotprod v1 v2

(*
let{n:nat} dotprod v1 v2 = begin
    loop (vect_length v1) 0 0
      where rec loop n sum i =
        if eq_int i n then sum else loop n (sum + (v1..(i) * v2..(i) : int)) (i+1)
    withtype int(n) -> int -> {i:nat | i <= n} int (i) -> int
end withtype int vect(n) -> int vect(n) -> int
;;

*)
