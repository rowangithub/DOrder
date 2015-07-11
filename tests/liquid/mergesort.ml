let ffor_up s d body =
  let rec loop i =
    if i <= d then (body i; loop (i+1)) else ()
  in loop s
in
let ffor_down s d body =
  let rec loop i =
    if i >= d then (body i; loop (i-1)) else ()
  in loop s
in
let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > Array.length a1 - len
             || ofs2 < 0 || ofs2 > Array.length a2 - len
  then assert false
  else if ofs1 < ofs2 then
    (* Top-down copy *)
    let bod i = 
      Array.set a2 (ofs2 + i) (Array.get a1 (ofs1 + i))
    in ffor_down (len-1) 0 bod 
  else
    (* Bottom-up copy *)
    let bod i =
      Array.set a2 (ofs2 + i) (Array.get a1 (ofs1 + i))
    in ffor_up 0 (len-1) bod
in
let cutoff = 5 in
let mergesort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len in
    let src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 then begin
        Array.set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (Array.get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        Array.set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (Array.get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (Array.get a src1ofs) src2ofs (Array.get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    let bod i = 
      let e = (Array.get a (srcofs + i)) in
      let rec wbod j = 
        if j >= dstofs then
          if not (cmp (Array.get dst j) e) then
            begin
              Array.set dst (j+1) (Array.get dst j);
              wbod (j-1)
            end
          else Array.set dst (j+1) e else Array.set dst (j+1) e
      in wbod (dstofs + i - 1)
    in ffor_up 0 (len-1) bod
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = Array.length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array.make l2 (Array.get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end;
in
let vec = [|20;19;18;17;16;15;14;13;12;11;10;9;8;7;6;5;4;3;2;1;0|] in
  mergesort (<=) vec; vec;;

    
(*let cutoff = 5;;
let stable_sort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (Array.get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (Array.get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (Array.get a src1ofs) src2ofs (Array.get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (Array.get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp (Array.get dst !j) e > 0) do
        set dst (!j + 1) (Array.get dst !j);
        decr j;
      done;
      set dst (!j + 1) e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = make l2 (Array.get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
 done;
*)
