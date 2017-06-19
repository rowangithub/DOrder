let rec loopb i j k = 
	if j < i then
		let k = k + i - j in
		let j = j + 1 in
		loopb i j k
	else k

let rec loopa i k n = 
	if i < n then
		let j = 0 in
		let k = loopb i j k in
		let i = i + 1 in
		loopa i k n
	else assert (k >= n)

let main n = 
	let k = 1 in
	let i = 1 in
	loopa i k n