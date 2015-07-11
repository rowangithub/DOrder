let rec loop p pathlim tmp = 
	if p < pathlim then
		let _ = assert (p <= tmp) in
		loop (p+1) pathlim tmp
	else ()

let glob2 pathbuf pathlim tmp =
	loop pathbuf pathlim tmp

let main () =
	let maxpathlen = 1 in
	let pathbuf = 0 in
	let bound = pathbuf + maxpathlen + 1 - 1 in
	let tmp = pathbuf + maxpathlen + 1 - 1 in
	glob2 pathbuf bound tmp
	
let _ = main ()