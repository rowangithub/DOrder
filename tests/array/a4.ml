let test (x: int) (y: int) (vec: int array) = 
		if (vec.(x) < vec.(x+1)) then
			let newmax = if (x >= vec.(y+1)) then x else vec.(y+1) in
			newmax
		else x
	

let main vec ith = 
	let n = Array.length vec in
	test 1 ith vec
	


let vec = [|1;2;3|]
let _ = main vec 1