let rec loopc (i:int) (j:int) (k:int) (ris:int) (u:int) (n:int) = 
	if (i<ris && j<u) then
		(*//assert(0<=i);assert(i<=n);
		//assert(0<=j);assert(j<=n);*)
		let i, j = 
			if (Random.bool ()) then
				(*//assert(0<=i);assert(i<=n);
				//assert(0<=k);assert(k<=n);*)
				i+1, j
			else
				(*//assert(0<=j);assert(j<=n);
				//assert(0<=k);assert(k<=n);*)	  
				i, j+1 in
		let k = k + 1 in	
		loopc i j k ris u n
	else 
		let _ = assert (k <= n) in
		k

let rec loopb z x n = 
	if z+x <= n then
		let y = z+x*2 in
		let y = if y > n then 
			let _ = assert(x>=1) in
			n+1 else y in
		let l = z in
		let ris = z + x in
		let u = y in
		let i, j, k = l, ris, l in
		let k = loopc i j k ris u n in
		(*//assert(0<=ris);assert(ris<=n);*)
  	(*while (i<ris) {
			//assert(0<=i);assert(i<=n);
			//assert(0<=k);assert(k<=n);
			//b[k]=a[i]; 
			i++; 
			k++;
		      }
  	while (j<u) { 
			//assert(0<=j);assert(j<=n);
			//assert(0<=k);assert(k<=n);
			//b[k]=a[j]; 
			j++; k++;
  	}
  	for (k=l; k<u; k++) { 
			//assert(0<=k);assert(k<=n);
			//a[k]=b[k]; 
  	}*)
		let z= z+x*2 in
		loopb z x n
	else ()
		

let rec loopa x n = 
	if x < n then
		let z = 1 in
		let _ = loopb z x n in
		let x = x * 2 in
		loopa x n
	else ()

let main n = 
	let x = 1 in
	loopa x n
	
let _ = main 4
let _ = main 3
let _ = main (-2)