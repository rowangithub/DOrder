let rec loopb t tagbuf_len = 
	if t = tagbuf_len then
		let _ = assert (0 <= t) in
		let _ = assert (t <= tagbuf_len) in ()
	else if (Random.bool ()) then
		if (Random.bool ()) then
			let _ = assert (0 <= t) in
			let _ = assert (t <= tagbuf_len) in		
			let t = t + 1 in
			if t = tagbuf_len then
				let _ = assert (0 <= t) in
				let _ = assert (t <= tagbuf_len) in
				()
			else (
				let _ = assert (0 <= t) in
				let _ = assert (t <= tagbuf_len) in
				let t = t + 1 in
				loopb t tagbuf_len
				)
		else (
			let _ = assert (0 <= t) in
			let _ = assert (t <= tagbuf_len) in
			let t = t + 1 in
			loopb t tagbuf_len
			)
	else if (Random.bool ()) then (
		assert (0 <= t);
		assert (t <= tagbuf_len)
		)
	else 
		let _ = assert (0 <= t) in
		let _ = assert (t <= tagbuf_len) in
		let t = t + 1 in
		loopb t tagbuf_len

let rec loopa t tagbuf_len = 
	if t = tagbuf_len then
		let _ = assert (0 <= t) in
		let _ = assert (t <= tagbuf_len) in
		()
	else if (Random.bool ()) then 
		let _ = assert (0 <= t) in
		let _ = assert (t <= tagbuf_len) in
		let t = t + 1 in
		loopb t tagbuf_len
	else 
		let _ = assert (0 <= t) in
		let _ = assert (t <= tagbuf_len) in
		let t = t + 1 in
		loopa t tagbuf_len
	
let main tagbuf_len = 
	if (tagbuf_len >= 1) then
		let t = 0 in
		let tagbuf_len = tagbuf_len - 1 in
		loopa t tagbuf_len
	else ()
	

let _ = main (-2)