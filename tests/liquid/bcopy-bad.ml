let rec bcopy_aux src des i m =
  if i > m then ()
  else
    begin
      let n = Array.get src i in
        Array.set des i n;
        let j = i + 1 in
          bcopy_aux src des j m
    end

let bcopy src des =
  let sz = Array.length src in
    bcopy_aux src des 0 sz

let driver =
  let sr = [|1; 2; 3|] in
  let asz = 3 in
  let ds = Array.make asz 0 in
    bcopy sr ds;;
