let rec loop j idBitLength material_length nelen = 
	if (8*j < (idBitLength)) && (j < material_length) then
		let _ = assert( 0 <= j) in
    let _ = assert( j < material_length ) in
    let _ = assert( 0 <= (j/4) ) in
    let _ = assert( (j) < (4*nelen)) in
		loop (j+1) idBitLength material_length nelen
    
  else ()

let main idBitLength material_length nelen =
	if (32 * nelen >= (idBitLength ) ) then 
		loop 0 idBitLength material_length nelen
	else ()
	

(*let _ = main 128 0 4
let _ = main 128 1 4
let _ = main 129 3 4
let _ = main 129 4 4
let _ = main 64 3 2
let _ = main 64 4 2*)
let _ = main (-2) (-2) (-2)