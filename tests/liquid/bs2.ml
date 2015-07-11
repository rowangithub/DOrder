let bs vec key =
	let rec bs_aux l u =
	    if u + 1 = l then -1
	    else
        let m = l + ((u-l) / 2) in
        let x = Array.get vec m in
		    if x < key then bs_aux (m+1) u
		    else if x > key then bs_aux l (m-1)
			 else m
    in
	bs_aux 0 (Array.length vec - 1)

let driver = 
  let _none = Random.self_init () in
  let ar = Array.make ((Random.int 40) + 2) 0 in
  let rec fill i = 
    if i < Array.length ar then  
      (Array.set ar i (Random.int 40); fill (i+1))
    else ()
  in
    (fill 0; bs ar (Random.int 40))
