let encrypt_ecb k b =
  (12, 11, 10, 9)

let encrypt_cfb8 k iv data =
  let l = Array.length data in
  let data' = Array.make l 0 in

  let sr = ref iv in (* shift register *)
    
  let loop i =
    if i <= l - 1 then begin
      let (v,_,_,_) = encrypt_ecb k !sr in
      let c = data.(i) lxor (v lsr 8) in
	data'.(i) <- c;
	let (sr3, sr2, sr1, sr0) = !sr in
	  sr := (((sr3 lsl 8) land 0xff00) lor (sr2 lsr 8),
	         ((sr2 lsl 8) land 0xff00) lor (sr1 lsr 8),
	         ((sr1 lsl 8) land 0xff00) lor (sr0 lsr 8),
	         ((sr0 lsl 8) land 0xff00) lor c);
    end
    else ()
  in loop 0;
  data'

let iv = (1, 2, 3, 4) 

let data = Array.make (Random.int 20 + 1) (Random.int 40) 

let driver =
  encrypt_cfb8 10 iv data
