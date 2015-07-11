type key =
    { data : int array;
      p_lsb : int array;
      p_msb : int array;
      (* subkeys: 18 elements of 16 bits *)
      p_lsb_rev : int array;
      p_msb_rev : int array;
      (* subkeys in reverse order *)
      s1_lsb : int array;
      s1_msb : int array;
      s2_lsb : int array;
      s2_msb : int array;
      s3_lsb : int array; 
      s3_msb : int array;
      s4_lsb : int array;
      s4_msb : int array;
      (* the four s-boxes: sNP, where N=0,1,2,3 denotes the box,
       * and P=0,1 whether the LSB (0) or MSB (1) of the box
       * value is stored.
       *)
    };;

let encrypt_ecb k (xl_msb,xl_lsb,xr_msb,xr_lsb) =
  (* The 64-bit element (xl, xr) is encrypted using key k with:
   * - xl_lsb the LSB of xl 
   * - xl_msb the MSB of xl 
   * - xr_lsb the LSB of xr 
   * - xr_msb the MSB of xr 
   * Returns (xl_msb', xl_lsb', xr_msb', xr_lsb'). 
   *)
  let p_lsb = k.p_lsb in
  let p_msb = k.p_msb in
  let s1_lsb = k.s1_lsb in
  let s1_msb = k.s1_msb in
  let s2_lsb = k.s2_lsb in
  let s2_msb = k.s2_msb in
  let s3_lsb = k.s3_lsb in
  let s3_msb = k.s3_msb in
  let s4_lsb = k.s4_lsb in
  let s4_msb = k.s4_msb in
  let rec compute_rounds i xl_lsb xl_msb xr_lsb xr_msb =
    if i < 16 then begin
      let xl_lsb' = xl_lsb lxor p_lsb.(i) in
      let xl_msb' = xl_msb lxor p_msb.(i) in
      let a = xl_msb' lsr 8 in
      let b = xl_msb' land 0xff in
      let c = xl_lsb' lsr 8 in
      let d = xl_lsb' land 0xff in
      let s_1a_plus_s_2b_lsb = 
	s1_lsb.(a) + s2_lsb.(b) in
      let s_1a_plus_s_2b_msb = 
	s1_msb.(a) + s2_msb.(b) + (s_1a_plus_s_2b_lsb lsr 16) in
      let after_xor_s_3c_lsb =
	(s_1a_plus_s_2b_lsb land 0xffff) lxor s3_lsb.(c) in
      let after_xor_s_3c_msb =
	(s_1a_plus_s_2b_msb land 0xffff) lxor s3_msb.(c) in
      let y_lsb = 
	after_xor_s_3c_lsb + s4_lsb.(d) in
      let y_msb =
	after_xor_s_3c_msb + s4_msb.(d) + (y_lsb lsr 16) in
      let xr_lsb' = (y_lsb land 0xffff) lxor xr_lsb in
      let xr_msb' = (y_msb land 0xffff) lxor xr_msb in
	compute_rounds (i+1) xr_lsb' xr_msb' xl_lsb' xl_msb' 
    end
    else
      (xr_msb lxor p_msb.(17),
       xr_lsb lxor p_lsb.(17),
       xl_msb lxor p_msb.(16),
       xl_lsb lxor p_lsb.(16))
	
  in compute_rounds 0 xl_lsb xl_msb xr_lsb xr_msb
in

let prepare key =
  let l_key = Array.length key in
    (* pmr: nothing to see here *)
    (*if l_key = 0 or l_key > 56 then
      failwith "Crypt_blowfish: invalid key length";*)

  let r = Random.int 20483 in
    let k =
      { data = key;
	p_lsb = Array.make 18 r;
	p_msb = Array.make 18 r;
	p_lsb_rev = Array.make 18 r;
	p_msb_rev = Array.make 18 r;
	s1_lsb = Array.make 256 r;
	s1_msb = Array.make 256 r;
	s2_lsb = Array.make 256 r;
	s2_msb = Array.make 256 r;
	s3_lsb = Array.make 256 r;
	s3_msb = Array.make 256 r;
	s4_lsb = Array.make 256 r;
	s4_msb = Array.make 256 r;
      } in
      
    let j = ref 0 in
    let loop1 i =
      if i <= 17 then
        let k0 = key.( !j ) in
	let k1 = key.( (!j + 1) mod l_key ) in
	let k2 = key.( (!j + 2) mod l_key ) in
	let k3 = key.( (!j + 3) mod l_key ) in
	  j := (!j + 4) mod l_key;
	  let d_msb = ( k0 lsl 8 ) lor k1 in
	  let d_lsb = ( k2 lsl 8 ) lor k3 in
	    k.p_lsb.( i ) <- k.p_lsb.( i ) lxor d_lsb;
	    k.p_msb.( i ) <- k.p_msb.( i ) lxor d_msb
      else ()
    in loop1 0;

      let d = ref (0,0,0,0) in

      let loop2 i =
        if i <= 8 then begin
	  d := encrypt_ecb k !d;
	  let (dl_msb, dl_lsb, dr_msb, dr_lsb) = !d in
	    k.p_lsb.( 2*i ) <- dl_lsb;
	    k.p_msb.( 2*i ) <- dl_msb;
	    k.p_lsb.( 2*i+1 ) <- dr_lsb;
	    k.p_msb.( 2*i+1 ) <- dr_msb;
        end
        else ()
      in loop2 0;

        let loop3 i =
          if i <= 127 then begin
            d := encrypt_ecb k !d;
	    let (dl_msb, dl_lsb, dr_msb, dr_lsb) = !d in
	      k.s1_lsb.( 2*i ) <- dl_lsb;
	      k.s1_msb.( 2*i ) <- dl_msb;
	      k.s1_lsb.( 2*i+1 ) <- dr_lsb;
	      k.s1_msb.( 2*i+1 ) <- dr_msb;
          end
          else ()
        in loop3 0;

        let loop4 i =
          if i <= 127 then begin
	    d := encrypt_ecb k !d;
	    let (dl_msb, dl_lsb, dr_msb, dr_lsb) = !d in
	      k.s2_lsb.( 2*i ) <- dl_lsb;
	      k.s2_msb.( 2*i ) <- dl_msb;
	      k.s2_lsb.( 2*i+1 ) <- dr_lsb;
	      k.s2_msb.( 2*i+1 ) <- dr_msb;
          end
          else ()
        in loop4 0;

        let loop5 i =
          if i <= 127 then begin
	    d := encrypt_ecb k !d;
	    let (dl_msb, dl_lsb, dr_msb, dr_lsb) = !d in
	      k.s3_lsb.( 2*i ) <- dl_lsb;
	      k.s3_msb.( 2*i ) <- dl_msb;
	      k.s3_lsb.( 2*i+1 ) <- dr_lsb;
	      k.s3_msb.( 2*i+1 ) <- dr_msb;
          end
          else ()
        in loop5 0;

        let loop6 i =
          if i <= 127 then begin
	    d := encrypt_ecb k !d;
	    let (dl_msb, dl_lsb, dr_msb, dr_lsb) = !d in
	      k.s4_lsb.( 2*i ) <- dl_lsb;
	      k.s4_msb.( 2*i ) <- dl_msb;
	      k.s4_lsb.( 2*i+1 ) <- dr_lsb;
	      k.s4_msb.( 2*i+1 ) <- dr_msb;
          end
          else ()
        in loop6 0;

        let loop7 i =
          if i <= 17 then begin
	    k.p_lsb_rev.( i ) <- k.p_lsb.( 17-i );
	    k.p_msb_rev.( i ) <- k.p_msb.( 17-i );
          end
          else ()
        in loop7 0;
        
        k
in
(*
let is_weak k = 
  (* A weak key is one in which two entries for a given S-box are identical
  *)
  (* Time: check takes 129540 loops. *)
  let check s_lsb s_msb =
    let rec outer_loop i =
      if i <= 254 then
        let a_lsb = s_lsb.(i) in
        let a_msb = s_msb.(i) in
        let rec inner_loop j =
          if j <= 255 then
	    begin if a_lsb = s_lsb.(j) & a_msb = s_msb.(j) then () end
          else ()
        in inner_loop (i + 1)
      else ()
    in outer_loop 0;
    ()
  in
    check k.s1_lsb k.s1_msb;
    check k.s2_lsb k.s2_msb;
    check k.s3_lsb k.s3_msb;
    check k.s4_lsb k.s4_msb
in
*)
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
in

let k = prepare (Array.make (Random.int 13) 0) in
let data = Array.make (Random.int 256) 0 in
let iv = (1, 2, 3, 4) in
  encrypt_cfb8 k iv data
;;
