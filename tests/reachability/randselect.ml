(** Implementation 1 *)

let rec append xs ys = 
	match xs with
		| x::xs' -> x::(append xs' ys)
		| [] -> ys

let rec extract acc n xs =
	match xs with 
  (*| [] -> raise Not_found*)
  | h :: t -> 
		if n = 0 then (h, append acc t) 
		else extract (h::acc) (n-1) t
		
let extract_rand list =
	match list with
		(*| [] -> raise Not_found*)
		| h::tl ->
			extract [] (Random.int (List.length list)) list
	
let rec aux n acc list =
  if n = 0 then acc 
	else
    let picked, rest = extract_rand list in
    aux (n-1) (picked :: acc) rest

let rec rand_select list n =
	match list with
		| x :: xs -> aux n [] list
		| [] -> list
		
let main () =
	let list = [3;5;4;8;1;6;2;7] in
	rand_select list 3
let _ = main ()	