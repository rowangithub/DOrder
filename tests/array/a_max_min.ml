let rec find i (n:int) minval maxval (a:int array) = 
	if (i+1 < n) then
		let tmp1 = a.(i) in
		let tmp2 = a.(i+1) in
		if (tmp1 < tmp2) then
			let newmax = if (maxval >= tmp2) then maxval else tmp2 in
			let newmin = if (minval <= tmp1) then minval else tmp1 in
			find (i+2) n newmin newmax a
		else 
			let newmax = if (maxval >= tmp1) then maxval else tmp1 in
			let newmin = if (minval <= tmp2) then minval else tmp2 in
			find (i+2) n newmin newmax a
	else if (i < n) then
		let tmp = a.(i) in
		let newmax = if (maxval >= tmp) then maxval else tmp in
		let newmin = if (minval <= tmp) then minval else tmp in
		find (i+1) n newmin newmax a
	else minval
	
(*let rec findmin i (n:int) minval (a:int array) =
	if (i < n) then
		if (a.(i) >= minval) then
			findmin (i+1) n minval a
		else findmin (i+1) n a.(i) a
	else minval
	
let rec findmax i (n:int) (maxval:int) (a:int array) =
	if (i < n) then
		if (a.(i) <= maxval) then
			findmax (i+1) n maxval a
		else findmax (i+1) n a.(i) a 
	else maxval*)
	
let main ith minval maxval vec = 
	let n = Array.length vec in
	let _ = find 0 n minval maxval vec in
	(*let minval = findmin 0 n minval vec in
	let maxval = findmax 0 n maxval vec in*)
	()
	
		
let vec = [|200; -300; -500; 900; -800|]
let _ = main 0 (-300) (200) vec		