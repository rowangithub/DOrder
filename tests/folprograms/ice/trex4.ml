let foo () = 
	let y=0 in
  let y = if (Random.bool ()) then y+1 else y in
	let y = if (Random.bool ()) then y-1 else y+10 in
  ()
	
let rec loop d x = 
	if (x > 0) then loop d (x-d)
	else assert (x <= 0)
		
let main x = 
  let d = 1 in

  let d = if (Random.bool ()) then d - 1 else d in
	let _ = if (Random.bool ()) then foo () else () in
  
  let _ = if (Random.bool ()) then foo () else () in
	let d = if (Random.bool ()) then d-1 else d in
  
  loop d x 
	

let _ = main 1