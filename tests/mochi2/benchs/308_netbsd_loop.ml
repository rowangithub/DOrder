let rec loop glob2_p_off glob2_pathlim_off maxpathlen = 
	if glob2_p_off <= glob2_pathlim_off then
		let _ = assert (0 <= glob2_p_off ) in
		let _ = assert (glob2_p_off < maxpathlen + 1) in
		loop (glob2_p_off+1) glob2_pathlim_off maxpathlen
	else ()


let main maxpathlen =
	if(maxpathlen > 0) then
		let pathbuf_off = 0 in
 	 	let bound_off = pathbuf_off + (maxpathlen + 1) - 1 in
		let glob2_pathbuf_off = pathbuf_off in
  	let glob2_pathlim_off = bound_off in
		loop glob2_pathbuf_off glob2_pathlim_off maxpathlen
	else ()
