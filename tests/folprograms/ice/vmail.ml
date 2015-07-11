let chars idx_in = 
	if idx_in <> 10 then Random.int 10 else 10

let rec loop idx_in i = 
	let c = chars idx_in in
	if (0 <= c && c <= 9) then
		let j = c - 0 in
		let i = i * 10 + j in
		let idx_in = idx_in + 1 in
		loop idx_in i
	else assert (i >= 0)

let main () =
  let idx_in = 0 in
  let i = 0 in
	loop idx_in i

let _ = main ()