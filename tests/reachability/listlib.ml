(** a set of useful list functions *)

let rec length xs = 
	match xs with
		| x :: xs' -> 1 + length xs'
		| [] -> 0	


let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []


let split list n =
    let rec split_aux i acc l =
			match l with
      | [] -> rev acc, []
      | h :: t -> 
				if i = 0 then 
					rev acc, l
        else split_aux (i-1) (h :: acc) t  in
    split_aux n [] list
		
		
let delete_at i al =
   let rec del i l =
      match l with
      | [] -> []
      | h::t -> 
				if i = 0 then t
      	else h :: del (i-1) t
   in
   del i al

		
let rec alternate xs ys = 
	match xs with
		| [] -> ys
		| x::xs' -> 
			x::(alternate ys xs') 


let rec concat xs ys = 
	match xs with
		| x::xs' -> x::(concat xs' ys)
		| [] -> ys


let rec extract acc n xs =
	match xs with 
  (*| [] -> raise Not_found*)
  | h :: t -> 
		if n = 0 then (h, concat acc t) 
		else extract (h::acc) (n-1) t
		
let extract_rand list =
	match list with
		(*| [] -> raise Not_found*)
		| h::tl ->
			extract [] (Random.int (List.length list)) list
	
let rec rand_select_aux n acc list =
  if n = 0 then acc 
	else
    let picked, rest = extract_rand list in
    rand_select_aux (n-1) (picked :: acc) rest

let rec rand_select list n =
	match list with
		| x :: xs -> rand_select_aux n [] list
		| [] -> list


let rec go xs ys = 
	match xs with
    | [] -> ys
    | x::xs' -> 
      if Random.bool () 
      then go xs' (x::ys)
      else go xs' ys
			
let random_subsequence xs = 
	rev (go xs [])	

  
let rotate list n =
	let len = length list in
  (* Compute a rotation value between 0 and len-1 *)
	let n = 
		if len = 0 then 0 
		else (n mod len + len) mod len in
	if n = 0 then list
	else 
		let a, b = split list n in 
		concat b a


let rec shuffle xs = 
	match xs with
		| [] -> []
		| x' :: xs' ->
			let xs = rev xs in
			match xs with
				| x' :: xs' -> 
					x' :: (shuffle xs')


let rec mem y list = 
	match list with
  	| [] -> false
  	| a::l -> 
			(a = y) || mem y l

let filter p list =
  let rec find accu l = 
		match l with
 			| [] -> rev accu
  		| x :: l -> 
				if p x then find (x :: accu) l 
				else find accu l in
  find [] list
	
let partition p list =
  let rec part yes no l = 
		match l with
  	| [] -> (rev yes, rev no)
  	| x :: l -> 
			if p x then part (x :: yes) no l 
			else part yes (x :: no) l in
  part [] [] list	
	
let f () = ()
let _ = f ()