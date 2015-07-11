(*
 * From CAV'12 by Sharma et al.
 *)
let rec loopa x y = 
	if (Random.bool ()) then loopa (x+1) (y+1)
	else x, y

let rec loopb x y n =
	if (x <> n) then loopb (x-1) (y-1) n
	else assert (y = n)

let main () =
  let x=0 in
  let y=0 in
  let n = 0 in
	let x, y = loopa x y in
	loopb x y n
	
let _ = main ()