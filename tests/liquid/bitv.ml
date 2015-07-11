let ffor s d body = 
    let rec loop i =
        let i' = i + 1 in 
        if i <= d then (body i; loop i') else () 
    in loop s

type t = { length : int; bits : int array }

let length v = v.length

(*s Each element of the array is an integer containing [30] bits, where
    [30] is determined according to the machine word size. Since we do not
    use the sign bit, [30] is 30 on a 32-bits machine and 62 on a 64-bits
    machines. We maintain the following invariant:
    {\em The unused bits of the last integer are always 
    zeros.} This is ensured by [create] and maintained in other functions
    using [normalize]. [bit_j], [bit_not_j], [low_mask] and [up_mask]
    are arrays used to extract and mask bits in a single integer. *)

let bpi0 = 32 - 2

let max_length = 32768 * 30

let bit_j = Array.init 30 (fun j -> 1 lsl j)

let bit_not_j = Array.init 30 (fun j -> max_int - bit_j.(j))

let low_mask = Array.make (30 + 1) 0

let _ =
  let loop i =
    if i <= 30 then
      low_mask.(i) <- low_mask.(i-1) lor bit_j.(i - 1)
    else ()
  in loop 1

let keep_lowest_bits a j = a land low_mask.(j)

let high_mask = Array.init (30 + 1) (fun j -> low_mask.(j) lsl (30-j))

let keep_highest_bits a j = a land high_mask.(j)

(*s Creating and normalizing a bit vector is easy: it is just a matter of
    taking care of the invariant. Copy is immediate. *)

let create n b =
  let initv = if b then max_int else 0 in
  let r = n mod 30 in
    if (n / 30) * 30 = n then
      { length = n; bits = Array.make (n / 30) initv }
    else begin
      let s = n / 30 in
      let b = Array.make (s + 1) initv in
        b.(s) <- b.(s) land low_mask.(r);
        { length = n; bits = b }
    end

let normalize v =
  let r = v.length mod 30 in
  if r > 0 then
    let b = v.bits in
    let s = Array.length b in
    b.(s-1) <- b.(s-1) land low_mask.(r)
  else ()

let copy v = { length = v.length; bits = Array.copy v.bits }

let pos n =
  let i = n / 30 in
  let _ = (fun (k: int) -> k) i in
  let j = n mod 30 in
  if j < 0 then (i - 1, j + 30) else (i,j)

(*s Access and assignment. The [n]th bit of a bit vector is the [j]th
    bit of the [i]th integer, where [i = n / 30] and [j = n mod
    30]. Both [i] and [j] and computed by the function [pos].
    Accessing a bit is testing whether the result of the corresponding
    mask operation is non-zero, and assigning it is done with a
    bitwiwe operation: an {\em or} with [bit_j] to set it, and an {\em
    and} with [bit_not_j] to unset it. *)

let unsafe_get v n =
  let (i,j) = pos n in
    ((Array.get v.bits i) land (Array.get bit_j j)) > 0

let unsafe_set v n b =
  let (i,j) = pos n in
  if b then
    Array.set v.bits i
      ((Array.get v.bits i) lor (Array.get bit_j j))
  else 
    Array.set v.bits i
      ((Array.get v.bits i) land (Array.get bit_not_j j))

(*s The corresponding safe operations test the validiy of the access. *)

let get v n =
  if n < 0 || n >= v.length then
    (* assert *) false
  else
    let (i,j) = pos n in
      ((Array.get v.bits i) land (Array.get bit_j j)) > 0

let set v n b =
  if n < 0 || n >= v.length then
    () (* assert false *)
  else
    let (i,j) = pos n in
      if b then
        Array.set v.bits i
          ((Array.get v.bits i) lor (Array.get bit_j j))
      else
        Array.set v.bits i
          ((Array.get v.bits i) land (Array.get bit_not_j j))

(*s [init] is implemented naively using [unsafe_set]. *)
let init n f =
  let v = create n false in
  let rec loop i =
    if i < n then begin
      unsafe_set v i (f i);
      loop (i + 1)
    end
    else ()
  in loop 0; v

(*s Handling bits by packets is the key for efficiency of functions
    [append], [concat], [sub] and [blit]. 
    We start by a very general function [blit_bits a i m v n] which blits 
    the bits [i] to [i+m-1] of a native integer [a] 
    onto the bit vector [v] at index [n]. It assumes that [i..i+m-1] and
    [n..n+m-1] are respectively valid subparts of [a] and [v]. 
    It is optimized when the bits fit the lowest boundary of an integer 
    (case [j == 0]). *)
let blit_bits a i m v n =
  let (i',j) = pos n in
  if j = 0 then
    Array.set v i'
      ((keep_lowest_bits (a lsr i) m) lor
       (keep_highest_bits (Array.get v i') (30 - m)))
  else
    let d = m + j - 30 in
    if d > 0 then begin
      Array.set v i'
	(((keep_lowest_bits (a lsr i) (30 - j)) lsl j) lor
	 (keep_lowest_bits (Array.get v i') j));
      Array.set v (i' + 1)
	((keep_lowest_bits (a lsr (i + 30 - j)) d) lor
	 (keep_highest_bits (Array.get v (i' + 1)) (30 - d)))
    end else
      Array.set v i'
	(((keep_lowest_bits (a lsr i) m) lsl j) lor
	 ((Array.get v i') land (low_mask.(j) lor high_mask.(0 - d))))

(*s [blit_int] implements [blit_bits] in the particular case when
    [i=0] and [m=30] i.e. when we blit all the bits of [a]. *)
(* assume n + 30 is in bounds *)
let blit_int a v n =
  let (i,j) = pos n in
  if j == 0 then
    Array.unsafe_set v i a
  else begin
    Array.unsafe_set v i
      ( (keep_lowest_bits (Array.get v i) j) lor
       ((keep_lowest_bits a (30 - j)) lsl j));
    Array.unsafe_set v (succ i)
      ((keep_highest_bits (Array.get v (succ i)) (30 - j)) lor
       (a lsr (30 - j)))
  end

(*s When blitting a subpart of a bit vector into another bit vector, there
    are two possible cases: (1) all the bits are contained in a single integer
    of the first bit vector, and a single call to [blit_bits] is the
    only thing to do, or (2) the source bits overlap on several integers of
    the source array, and then we do a loop of [blit_int], with two calls
    to [blit_bits] for the two bounds. *)

let unsafe_blit v1 ofs1 v2 ofs2 len =
  let (bi,bj) = pos ofs1 in
  let (ei,ej) = pos (ofs1 + len - 1) in
  if bi = ei then
    blit_bits (Array.get v1 bi) bj len v2 ofs2
  else begin
    if (30 - bj) < len then  (* ANNOT *)
      blit_bits (Array.get v1 bi) bj (30 - bj) v2 ofs2
    else ();
    let rec loop n i =
      if i <= ei - 1 then
        blit_int (Array.get v1 i) v2 n
      else begin
        if n + (ej + 1) <= Array.length v2 && 0 <= ei then (* ANNOT *)
          blit_bits (Array.get v1 ei) 0 (ej + 1) v2 n
        else ();
        loop (n + 30) (i+1)
      end
    in loop (ofs2 + 30 - bj) (bi + 1)
  end

let blit v1 v2 ofs1 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 + len > v1.length || ofs1 >= v1.length
             || ofs2 < 0 || ofs2 + len > v2.length || ofs2 >= v2.length
  then
    (* assert false *) ()
  else
    unsafe_blit v1.bits ofs1 v2.bits ofs2 len

(*s Extracting the subvector [ofs..ofs+len-1] of [v] is just creating a
    new vector of length [len] and blitting the subvector of [v] inside. *)

let sub v ofs len =
  if ofs < 0 || len <= 0 || ofs + len > v.length || ofs >= v.length then (* invalid_arg "Bitv.sub"; *) v
  else begin
  let r = create len false in
  unsafe_blit v.bits ofs r.bits 0 len;
  r
  end

(*s The concatenation of two bit vectors [v1] and [v2] is obtained by 
    creating a vector for the result and blitting inside the two vectors.
    [v1] is copied directly. *)

let append v1 v2 =
  let l1 = v1.length 
  and l2 = v2.length in
  let r = create (l1 + l2) false in
  let b1 = v1.bits in
  let b2 = v2.bits in
  let b = r.bits in
    ffor 0 (Array.length b1 - 1)
      (fun i ->
         Array.unsafe_set b i (Array.unsafe_get b1 i)
      );
    if l2 > 0 then
      unsafe_blit b2 0 b l1 l2 else ();
    r

(*s The concatenation of a list of bit vectors is obtained by iterating
    [unsafe_blit]. *)

let concat vl =
  let size = List.fold_left (fun sz v -> sz + v.length) 0 vl in
  let res = create size false in
  let b = res.bits in
  let pos = ref 0 in
  List.iter
    (fun v ->
       let n = v.length in
       let p = !pos in
         if n > 0 && p < size && p + n < size then (* ANNOT *)
           unsafe_blit v.bits 0 b p n
         else ();
       pos := !pos + n)
    vl;
  res

(*s Filling is a particular case of blitting with a source made of all
    ones or all zeros. Thus we instanciate [unsafe_blit], with 0 and
    [max_int]. *)

let blit_zeros v ofs len =
  let (bi,bj) = pos ofs in
  let (ei,ej) = pos (ofs + len - 1) in
  if bi = ei then
    blit_bits 0 bj len v ofs 
  else if (30 - bj) < len then  (* ANNOT *)
    begin
      blit_bits 0 bj (30 - bj) v ofs; 
      let n = ofs + 30 - bj in
       let n = 
        let rec loop i n =
          if i <= pred ei then (blit_int 0 v n; loop (i+1) (n+30)) else n in
        loop (succ bi) n in
       blit_bits 0 0 (succ ej) v n
   end else ()

let blit_ones v ofs len =
  let (bi,bj) = pos ofs in
  let (ei,ej) = pos (ofs + len - 1) in
  if bi == ei then
    blit_bits max_int bj len v ofs
  else if (30 - bj) < len then (* ANNOT *) 
    begin
    blit_bits max_int bj (30 - bj) v ofs;
    let n = ofs + 30 - bj in
    let n =
      let rec loop i n =
        if i <= pred ei then (blit_int max_int v n; loop (i+1) (n+30)) else n in
      loop (succ bi) n in
    blit_bits max_int 0 (succ ej) v n
  end else ()

let fill v ofs len b =
  if ofs < 0 || len < 0 || ofs + len > v.length then () else (*invalid_arg "Bitv.fill";*)
  if b then blit_ones v.bits ofs len else blit_zeros v.bits ofs len


(*s All the iterators are implemented as for traditional arrays, using
    [unsafe_get]. For [iter] and [map], we do not precompute [(f
    true)] and [(f false)] since [f] is likely to have
    side-effects. *)

let iter f v =
  let k = v.length in
  let rec loop i =
    if i >= k then () else begin f (unsafe_get v i); loop (i + 1) end
  in loop 0

let map f v =
  let l = v.length in
  let r = create l false in
  let rec loop i =
    if i >= l then () else begin unsafe_set r i (f (unsafe_get v i)) end
  in loop 0; r

let iteri f v =
  let k = v.length in
  let rec loop i =
    if i >= k then () else begin f i (unsafe_get v i); loop (i + 1) end
  in loop 0

let mapi f v =
  let l = v.length in
  let r = create l false in
  let rec loop i =
    if i >= l then () else begin unsafe_set r i (f i (unsafe_get v i)); loop (i + 1) end
  in loop 0; r

let fold_left f x v =
  let r = ref x in
  let k = v.length in
  let rec loop i =
    if i >= k then !r else begin r := f !r (unsafe_get v i); loop (i + 1) end
  in loop 0

let fold_right f v x =
  let r = ref x in
  let rec loop i =
    if i < 0 then !r else begin r := f (unsafe_get v i) !r; loop (i - 1) end
  in loop (v.length - 1)

let foldi_left f x v =
  let r = ref x in
  let k = v.length in
  let rec loop i =
    if i >= k then !r else begin r := f !r i (unsafe_get v i); loop (i + 1) end
  in loop 0

let foldi_right f v x =
  let r = ref x in
  let rec loop i =
    if i < 0 then !r else begin r := f i (unsafe_get v i) !r; loop (i - 1) end
  in loop (v.length - 1)

let iteri_true f v =
  Array.iteri 
    (fun i n -> if n != 0 then begin
       let i_30 = i * 30 in
       let rec loop j =
         if j < 30 then begin
	   if n land (Array.unsafe_get bit_j j) > 0 then f (i_30 + j) else ();
           loop (j + 1)
         end
         else ()
       in loop 0
     end else ())
    v.bits

(*s Bitwise operations. It is straigthforward, since bitwise operations
    can be realized by the corresponding bitwise operations over integers.
    However, one has to take care of normalizing the result of [bwnot]
    which introduces ones in highest significant positions. *)

let bw_and v1 v2 = 
  let l = v1.length in
    if l == v2.length then
      let b1 = v1.bits
      and b2 = v2.bits in
      let n = Array.length b1 in
      let a = Array.make n 0 in
      let rec loop i =
        if i < n then begin a.(i) <- b1.(i) land b2.(i); loop (i + 1) end else ()
      in loop 0; { length = l; bits = a }
    else assert false

let bw_or v1 v2 = 
  let l = v1.length in
    if l == v2.length then
      let b1 = v1.bits
      and b2 = v2.bits in
      let n = Array.length b1 in
      let a = Array.make n 0 in
      let rec loop i =
        if i < n then begin a.(i) <- b1.(i) lor b2.(i); loop (i + 1) end else ()
      in loop 0; { length = l; bits = a }
    else assert false

let bw_xor v1 v2 = 
  let l = v1.length in
    if l == v2.length then
      let b1 = v1.bits
      and b2 = v2.bits in
      let n = Array.length b1 in
      let a = Array.make n 0 in
      let rec loop i =
        if i < n then begin a.(i) <- b1.(i) lxor b2.(i); loop (i + 1) end else ()
      in loop 0; { length = l; bits = a }
    else assert false

let bw_not v =
  let b = v.bits in
  let n = Array.length b in
  let a = Array.make n 0 in
  let rec loop i =
    if i < n then begin a.(i) <- max_int land (lnot b.(i)); loop (i + 1) end else ()
  in loop 0; let r = { length = v.length; bits = a } in normalize r; r

(*s Shift operations. It is easy to reuse [unsafe_blit], although it is 
    probably slightly less efficient than a ad-hoc piece of code. *)

let rec shiftl v d =
  if d == 0 then 
    copy v
  else if d < 0 then
    shiftr v (-d)
  else begin
    let n = v.length in
    let r = create n false in
    if d < n then unsafe_blit v.bits 0 r.bits d (n - d) else ();
    r
  end

and shiftr v d =
  if d == 0 then 
    copy v
  else if d < 0 then
    shiftl v (-d)
  else begin
    let n = v.length in
    let r = create n false in
    if d < n then unsafe_blit v.bits d r.bits 0 (n - d) else ();
    r
  end

(*s Testing for all zeros and all ones. *)

let all_zeros v = 
  let b = v.bits in
  let n = Array.length b in
  let rec test i =
    if i == n then true else ((Array.unsafe_get b i == 0) && test (succ i))
  in test 0

let all_ones v = 
  let b = v.bits in
  let n = Array.length b in
  let rec test i = 
    if i == n - 1 then
      let m = v.length mod 30 in
      (Array.unsafe_get b i) == (if m == 0 then max_int else low_mask.(m))
    else
      ((Array.unsafe_get b i) == max_int) && test (succ i)
  in test 0

(*s Conversions to and from strings. *)

let to_string v = 
  let n = v.length in
  let s = String.make n '0' in
    ffor 0 (n-1) (fun i -> if unsafe_get v i then s.[i] <- '1' else ()); s

let print fmt v = Format.pp_print_string fmt (to_string v)

let of_string s =
  let n = String.length s in
  let v = create n false in
    ffor 0 (n - 1)
      (fun i ->
         let c = String.get s i in
           if c = '1' then
             unsafe_set v i true
           else
             if c <> '0' then (* invalid arg *) () else ()
      ); v

(*s Iteration on all bit vectors of length [n] using a Gray code. *)

let first_set v n = 
  let rec lookup i = 
    if i = n then (* assert false *) 0 else
      (if unsafe_get v i then i else lookup (i + 1))
  in 
  lookup 0

let gray_iter f n = 
  let bv = create n false in 
  let rec iter () =
    f bv;
    if 0 < n then begin (* ANNOT *)
      unsafe_set bv 0 (not (unsafe_get bv 0));
      f bv;
      let pos = succ (first_set bv n) in
        if pos < n then begin
          unsafe_set bv pos (not (unsafe_get bv pos));
          iter ()
        end else ()
    end else ()
  in
  if n > 0 then iter () else ()

(*s Coercions to/from lists of integers *)

let of_list l =
  let n = List.fold_left max 0 l in
    if n < 0 then (* ANNOT, though properly polymorphic max would fix this *)
      create 0 false
    else
      let b = create (succ n) false in
      let add_element i =
        (* negative numbers are invalid *)
        if i >= 0 then
          if i < n then (* ANNOT *)
            unsafe_set b i true
          else ()
        else ()
      in
        List.iter add_element l; b

let of_list_with_length l len =
  let b = create len false in
  let add_element i =
    if i < 0 || i >= len then
      (* assert false *) ()
    else
      let _ = (fun (s: int) -> s) i in
      unsafe_set b i true
  in
  List.iter add_element l; b

let to_list b =
  let n = length b in
  let rec make i acc = 
    if i < 0 then acc 
    else make (pred i) (if unsafe_get b i then i :: acc else acc)
  in
  make (pred n) []

(*s To/from integers. *)

(* [int] *)
let of_int_us i = 
  { length = 30; bits = [| i land max_int |] }

let to_int_us v = 
  if v.length < 30 then
    (* assert false *) 0
  else
    v.bits.(0)

let of_int_s i = 
  { length = succ 30; bits = [| i land max_int; (i lsr 30) land 1 |] }

let to_int_s v = 
  if v.length < succ 30 then
    (* assert false *) 0
  else
    v.bits.(0) lor (v.bits.(1) lsl 30)

(*
(* [Int32] *)
let of_int32_us i = match Sys.word_size with
  | 32 -> { length = 31; 
	    bits = [| (Int32.to_int i) land max_int; 
		      let hi = Int32.shift_right_logical i 30 in
		      (Int32.to_int hi) land 1 |] }
  | 64 -> { length = 31; bits = [| (Int32.to_int i) land 0x7fffffff |] }
  | _ -> assert false
let to_int32_us v =
  if v.length < 31 then invalid_arg "Bitv.to_int32_us"; 
  match Sys.word_size with
    | 32 -> 
	Int32.logor (Int32.of_int v.bits.(0))
	            (Int32.shift_left (Int32.of_int (v.bits.(1) land 1)) 30)
    | 64 ->
	Int32.of_int (v.bits.(0) land 0x7fffffff)
    | _ -> assert false

(* this is 0xffffffff (ocaml >= 3.08 checks for literal overflow) *)
let ffffffff = (0xffff lsl 16) lor 0xffff

let of_int32_s i = match Sys.word_size with
  | 32 -> { length = 32; 
	    bits = [| (Int32.to_int i) land max_int; 
		      let hi = Int32.shift_right_logical i 30 in
		      (Int32.to_int hi) land 3 |] }
  | 64 -> { length = 32; bits = [| (Int32.to_int i) land ffffffff |] }
  | _ -> assert false
let to_int32_s v =
  if v.length < 32 then invalid_arg "Bitv.to_int32_s"; 
  match Sys.word_size with
    | 32 -> 
	Int32.logor (Int32.of_int v.bits.(0))
	            (Int32.shift_left (Int32.of_int (v.bits.(1) land 3)) 30)
    | 64 ->
	Int32.of_int (v.bits.(0) land ffffffff)
    | _ -> assert false

(* [Int64] *)
let of_int64_us i = match Sys.word_size with
  | 32 -> { length = 63; 
	    bits = [| (Int64.to_int i) land max_int; 
		      (let mi = Int64.shift_right_logical i 30 in
		       (Int64.to_int mi) land max_int);
		      let hi = Int64.shift_right_logical i 60 in
		      (Int64.to_int hi) land 1 |] }
  | 64 -> { length = 63; 
	    bits = [| (Int64.to_int i) land max_int;
		      let hi = Int64.shift_right_logical i 62 in 
		      (Int64.to_int hi) land 1 |] }
  | _ -> assert false
let to_int64_us v = failwith "todo"
let of_int64_s i = failwith "todo"
let to_int64_s v = failwith "todo"

(* [Nativeint] *)
let select_of f32 f64 = match Sys.word_size with 
  | 32 -> (fun i -> f32 (Nativeint.to_int32 i))
  | 64 -> (fun i -> f64 (Int64.of_nativeint i))
  | _ -> assert false
let of_nativeint_s = select_of of_int32_s of_int64_s
let of_nativeint_us = select_of of_int32_us of_int64_us
let select_to f32 f64 = match Sys.word_size with 
  | 32 -> (fun i -> Nativeint.of_int32 (f32 i))
  | 64 -> (fun i -> Int64.to_nativeint (f64 i))
  | _ -> assert false
let to_nativeint_s = select_to to_int32_s to_int64_s
let to_nativeint_us = select_to to_int32_us to_int64_us

*)
