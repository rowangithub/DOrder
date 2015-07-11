let rec length xs = 
	match xs with
		| x :: xs' -> 1 + length xs'
		| [] -> 0

let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys

let reverse l =
	let rec aux_rev us ys = 
		match us with
			| [] -> ys
			| x::zs -> aux_rev zs (x::ys) in
    aux_rev l []

let split list n =
    let rec aux_split i acc l = 
			match l with
      | [] -> reverse acc, []
      | h :: t -> 
				if i = 0 then reverse acc, l
        else aux_split (i-1) (h :: acc) t  in
    aux_split n [] list
  
let rotate list n =
	let len = length list in
  (* Compute a rotation value between 0 and len-1 *)
	let n = 
		if len = 0 then 0 
		else (n mod len + len) mod len in
	if n = 0 then list
	else 
		let a, b = split list n in 
		append b a
		
let main () = 
	rotate [3;2;7;5;8;4;6;1;] 3
let _ = main ()