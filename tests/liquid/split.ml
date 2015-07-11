let rec quickSort arr a n =
    let item i = Array.get arr i in
    let swap i j =
      let tmp = Array.get arr i in 
      let ij = Array.get arr j in
      (Array.set arr i ij; Array.set arr j tmp)
    in
    let rec bottom limit pa pb = 
      (*let _ = (fun x y -> (x, y)) pa pb in*)
			let arg = {p1 = pa; p2 = pb} in
      let ia = item a in
      (*let _ = (fun x -> x) pa in
      let _ = (fun x -> x) pb in*)
      let pb' = pb + 1 in
      let pa' = pa + 1 in
      if limit < pb then arg else
      let ipb = item pb in
			if ia < ipb then arg else
				if ipb < ia then bottom limit pa pb' else
						(swap pa pb; bottom limit pa' pb')
		in 
    let rec top limit pc pd = 
			let arg = {p1 = pc; p2 = pd} in
      let pc' = pc - 1 in
      let pd' = pd - 1 in
      if pc < limit then arg else
      if item pc < item a then arg else
			if item a < item pc then top limit pc' pd else
			(swap pc pd; top limit pc' pd') 
    in 
    let rec split pa pb pc pd =
			let papb = bottom pc pa pb in
      let pa = papb.p1 in
      let pb = papb.p2 in
      let pcpd = top pb pc pd in
      let pc = pcpd.p1 in
      let pd = pcpd.p2 in
      let _ = (fun x -> x) pa in
      let _ = (fun x -> x) pb in
      let _ = (fun x -> x) pc in
      let _ = (fun x -> x) pd in
      if pb >= pc then {q1 = pa; q2 = pb; q3 = pc; q4 = pd}
      else 
      (swap pb pc; 
								 split pa (pb+1) (pc-1) pd) 
 	  in 
    let a' = a + 1 in
    let an = a + n in
    let an' = an - 1 in
    let spllit = split a' a' an' an' in
    let pa = spllit.q1 in
    let pb = spllit.q2 in
    let pc = spllit.q3 in
    let pd = spllit.q4 in
    let _ = (fun x -> x) pa in
    let _ = (fun x -> x) pb in
    let _ = (fun x -> x) pc in
    let _ = (fun x -> x) pd in
      (fun x -> x) spllit 

let x = let x : garbage = 0 in x in
let y = Random.int 20 + 1 in
let vec = Array.make y x in
let _ = (fun x -> x) vec in
  quickSort vec 0 (y-1);;
