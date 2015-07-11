let main x0 x1 x2 x3 x4 = 
	let lock = 0 in
	let x0 = if (Random.bool ()) then x0 + 1 else x0 - 1 in
	let x1 = if (Random.bool ()) then x1 + 1 else x1 - 1 in
	let x2 = if (Random.bool ()) then x2 + 1 else x2 - 1 in
  let x3 = if (Random.bool ()) then x3 + 1 else x3 - 1 in
	let x4 = if (Random.bool ()) then x4 + 1 else x4 - 1 in
	let _ = assert (lock = 0) in
	()
let _ = main 0 0 0 0 0