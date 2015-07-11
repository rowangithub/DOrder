qualifier DEAD(x) = not(true);;

let y = 3 in
let x = 3 in
  if y = x then
    let k = y + 1 in
      if x < k then
        1
      else
        0
  else
    -1;;
