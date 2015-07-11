let rec loopa flag a b s t = 
		if ((Random.bool ())) then
			let a = a+1 in
			let b = b+1 in
			let s = s+a in
			let t = t+b in
			let t = if (flag > 0) then t+a else t in
			loopa flag a b s t
		else 
			(
			s, t)	

let rec loopb x y = 
		if y <= x then
			let y = if ((Random.bool ())) then y+1 else y+2 in
			loopb x y
		else assert (y <= 4)

let main flag = 
  let t = 0 in
  let s = 0 in
  let a = 0 in
  let b = 0 in
	let s, t = loopa flag a b s t in
  let x = 1 in
  let x = 
		if(flag > 0) then t-2*s+2 else x in
  let y = 0 in
	loopb x y

let _ = main 5
let _ = main (-5)
let _ = main 4
let _ = main (-4)
let _ = main 3
let _ = main (-3)
let _ = main 2
let _ = main (-2)
let _ = main 1
let _ = main (-1)
let _ = main 6
let _ = main (-6)
let _ = main 7
let _ = main (-7)
let _ = main 8
let _ = main (-8)