let rec sum n = 
  if n < 0 then 0 else 
    let t = sum (n-1) in
    n + t 


