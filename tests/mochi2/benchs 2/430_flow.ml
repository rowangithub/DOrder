let lamp x = x
let f =
  let id x = x in
  let unused = id (fun _ -> assert false) in
    lamp

let main i = f ()
