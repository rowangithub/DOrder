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

let encrypt_ecb _ _ = (1, 2, 3, 4) in

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
in prepare (Array.make (Random.int 255 + 1) (Random.int 255));;
