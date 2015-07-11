let test (a: int) b = assert(a < b)

let produce (c:int) d = (c, d)

let main () = 
	let (m, n) = produce 0 1 in
	test m n

let _ = main ()