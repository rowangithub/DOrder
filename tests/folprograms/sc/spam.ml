let rec loopb i j len limit bufsize = 
	if (i < len && j < limit) then (
		if (i + 1 < len) then (
			assert(i+1<len);(*//1*)
			assert(0<=i);(*//2//Interesting assert*)
			if (Random.bool ()) then
				(assert(i<len);(*//12*)
				assert(0<=i);(*//Really really interesting*)
	    	assert(j<bufsize);(*//13*)
				assert(0<=j);	(*//14*)
	
				(*//	buffer[j] = msg[i];*)
	    	let j = j+1 in
	    	let i = i+1 in
				loopb i j len limit bufsize)
			else	
				(assert(i<len);(*//3*)
				assert(0<=i); (*//4*)
				assert(j<bufsize);(*//5 //Interesting Assert*)
				assert(0<=j);	
				(*//        buffer[j] = msg[i];*)
				let j = j + 1 in
				let i = i + 1 in
				let _ = assert(i<len) in(*//6*)
				let _ = assert(0<=i) in(*//7*)
				let _ = assert(j<bufsize) in(*//8 //Very intersting*)
				let _ = assert(0<=j) in	(*//9*)
		
				(*//        buffer[j] = msg[i];*)
				let j = j + 1 in
				let i = i + 1 in
				let _ = assert(j<bufsize) in(*//10*)
				let _ = assert(0<=j) in	(*//11*)
				(*/* OK */
				//        buffer[j] = '.';*)
				let j = j + 1 in
				loopb i j len limit bufsize)
		)
	  else (
    	assert(i<len);(*//12*)
			assert(0<=i);(*//Really really interesting*)
    	assert(j<bufsize);(*//13*)
			assert(0<=j);	(*//14*)

			(*//	buffer[j] = msg[i];*)
    	let j = j+1 in
    	let i = i+1 in
			loopb i j len limit bufsize
	  )
	)	
	else i

let rec loopa i len limit bufsize = 
	if (i < len) then
		let i = loopb i 0 len limit bufsize in
		loopa i len limit bufsize
 	else ()

let main len bufsize = 
  (*  char buffer[BUFSZ];*)
  let limit = bufsize - 4 in
	loopa 0 len limit bufsize
	
let _ = main 7 5
let _ = main 5 7
let _ = main 6 5
let _ = main 5 6
let _ = main (-5) (-5)