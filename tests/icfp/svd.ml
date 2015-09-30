let rec loopa i j l m n = 
	if j <= n then
		(assert(1<=j);assert(j<=n);
  	assert(1<=i);assert(i<=n);
  	(*//assert(1<=i);assert(i<=m); // TODO feasible counterexample found*)
  	assert(1<=l);assert(l<=n);
		loopa i (j+1) l m n)
	else ()
					
let rec loopc i j k l m n = 
	if k <= n then 
		((*//assert(1<=i);assert(i<=m); // TODO feasible counterexample found*)
		assert(1<=k);assert(k<=n);
		assert(1<=j);assert(j<=n);
		loopc i j (k+1) l m n)
	else ()		
							
let rec loopd i j k l m n =
	if k <= n then 
		(assert(1<=k);assert(k<=n);
		assert(1<=j);assert(j<=n);
		assert(1<=i);assert(i<=n);
		loopd i j (k+1) l m n)
	else ()			

let rec loopb i j l m n = 
	if j <= n then 
		(loopc i j l l m n;
		loopd i j l l m n;
		loopb i (j+1) l m n)	
	else ()

let rec loope i j l m n = 
	if j <= n then 
		(assert(1<=j);assert(j<=n);
		assert(1<=i);assert(i<=n);
		loope i (j+1) l m n)
	else ()
				
let rec loop i l m n = (* Accumulation of right-hand transformations. *)
	if (i >= 1) then	
    ((if (i < n) then (
      (if (Random.bool ()) then (
				loopa i l l m n; (* Double division to avoid possible underflow. *)
				loopb i l l m n	
      ) 
			else ());
			loope i l l m n
    ) else ()); 
   	assert(1<=i);
    assert(i<=n);
    assert(1<=i);assert(i<=n);
		loop (i-1) i m n)
	else ()
						
let main l m n =
  if (l>0) then	
		loop n l m n
	else ()
	

let _ = main (-1) 0 (-2)