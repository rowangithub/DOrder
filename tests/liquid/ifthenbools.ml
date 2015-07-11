qualif S2(x) : Array.length x = 2;; 
qualif LT1(x) : x < 2;;
qualif GE0(x) : x >= 0;;

let bug arr =
  let get i = Array.get arr i in
  let comp i j =
    let ig = get i in
      let ij = get j in
        if ig < ij then () else ()
  in
    comp 0 1 
in
let arr = Array.make 2 0 in
bug arr;;
