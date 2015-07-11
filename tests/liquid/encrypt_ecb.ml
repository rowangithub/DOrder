
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

let sk = Array.make 18 (Random.int 0xffff) in
let sb = Array.make 256 (Random.int 0xffff) in
let data = Array.make (Random.int 100 + 1) (Random.int 200) in
let k =
  { data = data; p_lsb = sk; p_msb = sk; p_lsb_rev = sk; p_msb_rev = sk;
    s1_lsb = sb; s1_msb = sb; s2_lsb = sb; s2_msb = sb;
    s3_lsb = sb; s3_msb = sb; s4_lsb = sb; s4_msb = sb } in
  encrypt_ecb k (1, 2, 3, 4);;
