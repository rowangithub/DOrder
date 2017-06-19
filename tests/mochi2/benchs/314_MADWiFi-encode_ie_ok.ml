let rec loop i p ielen bufsize (bufsizecopy:int) (leaderlen:int) = 
	if i < ielen && bufsize > 2 then
		let _ = assert (0 <= p) in
		let _ = assert (p+1 < bufsizecopy) in
		(*//    *p = 'x';
      	//    *(p+1) = 'x';*)
		loop (i+1) (p+2) ielen bufsize bufsizecopy leaderlen
	else ()

let main leaderlen bufsize ielen =
  (* copy the contents of leader into buf *)	
	if (leaderlen > 0 && bufsize > 0 && ielen > 0 && bufsize >= leaderlen) then	
  	(*  p = buf;*)
		let p = 0 in
  	(*  memcpy(p, leader, leader_len);*)
  	let bufsizecopy = bufsize in
  	let bufsize = bufsize - leaderlen in
  	let p = p + leaderlen in

  	(* This is the fix. *)
  	if (bufsize >= 2*ielen) then 
			let _ = assert (p+ielen<=bufsizecopy) in
			loop 0 p ielen bufsize bufsizecopy leaderlen
    else()
	else ()