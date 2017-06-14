let rec loop mc_i (n1:int) n2 cp1_off maxdata = 
	if mc_i < n2 then
		(assert (cp1_off+mc_i < maxdata * 2);
		loop (mc_i+1) n1 n2 cp1_off maxdata)
	else ()
	

let main n1 n2 cp1_off maxdata = 
  if (maxdata > 0 && n1 <= maxdata * 2 && cp1_off <= n1 &&
				n2 <= maxdata*2 - n1) then
					
		loop 0 n1 n2 cp1_off maxdata				
	
	else ()
	

let _ = main 1 2 3 4
let _ = main (-1) (-3) (-5) (-7) 