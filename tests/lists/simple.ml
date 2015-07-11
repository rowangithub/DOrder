(*let rec mapfilter = fun f -> fun l ->
  match l with
      [] ->
	[]
    | h::t ->
	let r = mapfilter f t in
	let x = f h in
	  match x with
              [] ->
                r
            | z::e ->
                z::r
*)
		
let rec mapfilter (f:int->int list) (l:int list) = match l with
	| [] -> []
	| h::t -> 
		let r = mapfilter f t in
		let x = f h in
		match x with
			| [] -> r
			| z::e -> (z::r)		

let pos = fun y ->
  if 0 < y then
    y::[]
  else
    []

let neg = fun w ->
  if w < 0 then
    w::[]
  else
    []

(*let rec f xs = match xs with
	| x::xs -> (assert (x >= 0); f xs)
	| [] -> ()*)

let main () = 
	mapfilter pos (1::2::1::[])
	
let _ = main ()