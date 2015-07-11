type mysum = N of unit | I of int | M

let g z = 
  match z with
  | I i -> () 
  (*| _ -> ()
  | N () -> () 
  | M -> ()*)


