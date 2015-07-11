let rec go xs ys = match xs with
    | [] -> ys
    | x::xs' -> 
      if Random.bool () 
      then go xs' (x::ys)
      else go xs' ys
			
let rev list =
	let rec aux2 acc t = 
		match t with
  	| [] -> acc
    | h::t -> aux2 (h::acc) t in
  aux2 [] list
			
let random_subsequence xs = 
	rev (go xs [])	
		
let main () =
	let list = [3;5;4;8;1;6;2;7] in
	random_subsequence list
let _ = main ()	