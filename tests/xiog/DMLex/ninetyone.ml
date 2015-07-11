fun f91 (x) =
  if (x <= 100) then f91 (f91 (x + 11)) else x - 10
withtype {i:int} <max(0, 101-i)> => 
         int(i) -> 
         [j:int | (i<=100 /\ j=91) \/ (i>=101 /\ j=i-10)] int(j)
