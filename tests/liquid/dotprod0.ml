let dotprod v = 
  let rec loop n sum i =
    if i = n then sum else
      let x = Array.get v i in
      loop n (x*x + sum) (i+1) in
    loop (Array.length v) 0 0 
