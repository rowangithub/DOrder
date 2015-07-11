let rec bcopy_aux src des i l j =
  if i = l then ()
  else
    let n = Array.get src i in
      Array.set des j n;
      let iprm = i + 1 in
      let jprm = j + 1 in
      bcopy_aux src des iprm l jprm

let bcopy src des ss len ds =
  let se = ss + len in
  let srcl = Array.length src in
    if (se <= srcl) then
      let de = ds + len in
      let desl = Array.length des in
        if (de <= desl) then
          let fini = ss + len in
            bcopy_aux src des ss fini ds
        else ()
    else ()

let driver =
  let _none = Random.init 555 in
  let asz = Random.int 20 in
  let aszprm = asz + 2 in
  let bsz = Random.int 20 in
  let bszprm = bsz + 1 in
  let len = Random.int 30 in
  let src = Array.make aszprm 1 in
  let des = Array.make bszprm 0 in
    bcopy src des 1 len 0

(*let src = [|1; 2; 3|] in
let asz = 3 in
let des = Array.make asz 0 in
  bcopy src des 1 2 0;; *)
