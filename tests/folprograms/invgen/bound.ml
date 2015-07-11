let rec loopa i k h = 
	if (i > 0) then
		loopa (i-1) (k+1) (i+k)
	else (assert (i >= 0); i, k, h)

let rec loopb j m h = 
	if (j > 0) then
		loopb (j-1) (m+1) (j+m)
	else ()

let main n =
	if (n >= 0 && n <= 200) then  
  
  	let k=0 in
  	let i=n in
  	let h = i+k in
  	let i, k, h = loopa i k h in

  	let j = k in
  	let m = 0 in
  	let h = j+m in
  	loopb j m h
	else ()
		
let _ = main 10
let _ = main 201
let _ = main (-1)