let rec loop i j k n b = 
	if (n < 2 * k) then
		let n = n + 1 in
		let i, j = 
			if (b = 1) then i+1, j else i, j+1 in
		let b = 1 - b in
		loop i j k n b
	(* Hack: add n mod 2 = 0 as a predicate *)	
	else if (n mod 2 = 0) then assert (i = j)
	else assert (i = j)

(* add a qualifier b = 0 || b = 1 to 
  encode boolean variables using integer *)
let main j b =
  let k = 100 in
  let i = j in
	loop i j k 0 b
	
let _ = main 0 1
let _ = main 0 0
let _ = main 1 1
let _ = main 1 0