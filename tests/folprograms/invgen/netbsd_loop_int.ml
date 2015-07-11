(*
 * "NetBSD_loop_int" from InvGen benchmark suite
 *)

let rec loop maxpathlen glob3_pathend_off glob3_dc glob3_pathlim_off = 
    if (glob3_pathend_off + glob3_dc >= glob3_pathlim_off) then ()
    else 
      let glob3_dc = glob3_dc + 1 in
      let _ = assert (0 <= glob3_dc) in
     	let _ = assert (glob3_dc < maxpathlen + 1) in
      if (Random.bool ()) then ()
			else loop maxpathlen glob3_pathend_off glob3_dc glob3_pathlim_off
  
		
let main maxpathlen =
  if(maxpathlen > 0) then
		let buf_off = 0 in
  	let pattern_off = 0 in
		let bound_off = 0 + (maxpathlen + 1) - 1 in
		let glob3_pathbuf_off = buf_off in
  	let glob3_pathend_off = buf_off in
  	let glob3_pathlim_off = bound_off in
  	let glob3_pattern_off = pattern_off in
		let glob3_dc = 0 in
  	loop maxpathlen glob3_pathend_off glob3_dc glob3_pathlim_off
	else ()
	
let _ = main 10
let _ = main 9
let _ = main (-10)