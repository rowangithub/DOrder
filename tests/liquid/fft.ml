let pi = 3.14 
(*
** by: Dave Edelblute, edelblut@cod.nosc.mil, 05 Jan 1993
** Modified: R. Mayer to work with hist benchmark routines.
** Translated from C to de Caml: Hongwei Xi, 07 Nov 1998
*)
let two_pi = 2.0 *. pi  

let ffor s d body = 
    let rec loop i =
        let i' = i + 1 in 
        if i <= d then (body i; loop i') else () 
    in loop s

let fft px py n = (* n must be a power of 2! *)
  let rec loop n2 n4 =
    if n2 <= 2 then () else (* the case n2 = 2 is treated below *)
    let e = two_pi /. (float_of_int n2) in
    let e3 = 3.0 *. e in
    let a = ref 0.0 in
    let a3 = ref 0.0 in
    let forbod j = 
    (*for j = 1 to n4 do*)
      let cc1 = cos !a in
      let ss1 = sin !a in
      let cc3 = cos !a3 in
      let ss3 = sin !a3 in
      let b_a = !a in
      let b_a3 = !a3 in
      let none_ = a := b_a +. e in 
      let none_ = a3 := b_a3 +. e3 in
      let rec loop1 i0 i1 i2 i3 id =
        if i3 > n then () else (* out_of_bounds *)
        let g_px_i0 = Array.get px i0 in
        let g_px_i2 = Array.get px i2 in
        let r1 = g_px_i0 -. g_px_i2 in
        let r1' = g_px_i0 +. g_px_i2 in
        let none_ = Array.set px i0 r1' in
        let g_px_i1 = Array.get px i1 in
        let g_px_i3 = Array.get px i3 in
        let r2 = g_px_i1 -. g_px_i3 in
        let r2' = g_px_i1 +. g_px_i3 in
        let none_ = Array.set px i1 r2' in
        let g_py_i0 = Array.get py i0 in
        let g_py_i2 = Array.get py i2 in
        let s1 = g_py_i0 -. g_py_i2 in
        let s1' = g_py_i0 +. g_py_i2 in
        let none_ = Array.set py i0 s1' in
        let g_py_i1 = Array.get py i1 in
        let g_py_i3 = Array.get py i3 in
        let s2 = g_py_i1 -. g_py_i3 in
        let s2' = g_py_i1 +. g_py_i3 in
        let none_ = Array.set py i1 s2' in
        let s3 = r1 -. s2 in 
        let r1 = r1 +. s2 in
        let s2 = r2 -. s1 in
        let r2 = r2 +. s1 in
        let none_ = Array.set px i2 (r1 *. cc1 -. s2 *. ss1) in
        let none_ = Array.set py i2 ((-. s2) *. cc1 -. r1 *. ss1) in
        let none_ = Array.set px i3 (s3 *. cc3 +. r2 *. ss3) in
        let none_ = Array.set py i3 (r2 *. cc3 -. s3 *. ss3) in
        loop1 (i0 + id) (i1 + id) (i2 + id) (i3 + id) id
      in
      let rec loop2 is id =
        if is >= n then () else begin
          let i1 = is + n4 in
          let i2 = i1 + n4 in
          let i3 = i2 + n4 in
          loop1 is i1 i2 i3 id;
          loop2 (2 * id - n2 + j) (4 * id)
        end
      in
      loop2 j (2 * n2)
    in ffor 1 n4 forbod; 
    loop (n2 / 2) (n4 / 2) in
    loop n (n / 4);

    let rec loop1 i0 i1 id =
      if i1 > n then () else
      let r1 = Array.get px i0 in
      let none_ = Array.set px i0 (r1 +. (Array.get px i1)) in
      let none_ = Array.set px i1 (r1 -. (Array.get px i1)) in
      let r1 = Array.get py i0 in
      let none_ = Array.set py i0 (r1 +. (Array.get py i1)) in
      let none_ = Array.set py i1 (r1 -. (Array.get py i1)) in
      loop1 (i0 + id) (i1 + id) id
    in
    let rec loop2 is id =
      if is >= n then () else begin
        loop1 is (is + 1) id;
        loop2 (2 * id - 1) (4 * id)
      end
    in
    loop2 1 4;

    let rec loop1 j k =
      if k >= j then j + k else loop1 (j - k) (k / 2)
    in
    let rec loop2 i j =
      if i >= n then () else begin
        if i >= j then () else begin
          let xt = Array.get px j in (Array.set px j (Array.get px i); Array.set px i (xt));
          let xt = Array.get py j in (Array.set py j (Array.get py i); Array.set py i (xt))
        end;
        loop2 (i + 1) (loop1 j (n / 2))
      end
    in
    loop2 1 1; n

(* pmr: is this going to cause a problem? *)
(* ming: we aren't trying to prove anything over floats? *)
let fabs r = if r > 0.0 then r else (0.0 -. r)
                                     
let ffttest np =
  let enp = float_of_int np in
  let n2 = np / 2 in
  let npm = n2 - 1 in
  let pxr = Array.make (np+1) 0.0 in
  let pxi = Array.make (np+1) 0.0 in
  let t = pi /. enp in
  let none_ = Array.set pxr 1 ((enp -. 1.0) *. 0.5) in
  let none_ = Array.set pxi 1 (0.0) in
  let none_ = Array.set pxr (n2+1) ((-. (1.0 *. 0.5))) in
  let none_ = Array.set pxi (n2+1) (0.0) in
  let forbod i = 
    let j = np - i in
    let none_ = Array.set pxr (i+1) (-. (1.0 *. 0.5)) in
    let none_ = Array.set pxr (j+1) (-. (1.0 *. 0.5)) in
    let z = t *. (float_of_int i) in
    let y = (cos z /. sin z) *. 0.5 in
    Array.set pxi (i+1) (-. y); Array.set pxi (j+1) (y)
  in ffor 1 npm forbod;
  ignore (fft pxr pxi np);
  let rec loop i zr zi kr ki =
    if i >= np then (zr, zi) else
      let a = fabs((Array.get pxr (i+1)) -. (float_of_int i)) in
      let b = zr < a in
      let zr = if b then a else zr in
      let kr = if b then i else kr in
      let a = fabs(Array.get pxi (i+1)) in
      let b = zi < a in
      let zi = if b then a else zi in
      let ki = if b then i else ki in
       loop (i+1) zr zi kr ki
  in
  let zz = loop 0 0.0 0.0 0 0 in
  let zr = fst zz in
  let zi = snd zz in
  let zm = if fabs zr < fabs zi then zi else zr
  in
  (*in print_float zm; print_newline ()*) zm

let rec loop_np i np =
  if i > 16 then () else begin ignore (ffttest np); loop_np (i + 1) (np * 2) end

let doit _none = loop_np 4 16 

let driver = doit ()

(*
(*
** by: Dave Edelblute, edelblut@cod.nosc.mil, 05 Jan 1993
** Modified: R. Mayer to work with hist benchmark routines.
** Translated from C to de Caml: Hongwei Xi, 07 Nov 1998
*)

let pi = 3.14159265358979323846 in
let two_pi = 2 * pi in

let{n:int | n >= 2} fft px py n = (* n must be a power of 2! *)
  let rec{n2:nat} loop n2 n4 =
    if le_int n2 2 then () else (* the case n2 = 2 is treated below *)
    let e = two_pi /. (float_of_int n2) in let e3 = 3.0 *. e in
    let a = ref 0.0 and a3 = ref 0.0 in
    for j = 1 to n4 do
      let cc1 = cos !a and ss1 = sin !a and cc3 = cos !a3 and ss3 = sin !a3 in
      let none_ = a := !a +. e and none_ = a3 := !a3 +. e3 in
      let rec loop1 i0 i1 i2 i3 id =
        if gt_int i3 n then () else (* out_of_bounds *)
        let r1 = px..(i0) -. px..(i2)
        and none_ = Array.set px i0 (px..(i0) +. px..(i2))
        and r2 = px..(i1) -. px..(i3)
        and none_ = Array.set px i1 (px..(i1) +. px..(i3))
        and s1 = py..(i0) -. py..(i2)
        and none_ = Array.set py i0 (py..(i0) +. py..(i2))
        and s2 = py..(i1) -. py..(i3)
        and none_ = Array.set py i1 (py..(i1) +. py..(i3) in)
        let s3 = r1 -. s2 and r1 = r1 +. s2
        and s2 = r2 -. s1 and r2 = r2 +. s1 in
        let none_ = px..(i2) <- r1 *. cc1 -. s2 *. ss1
        and none_ = py..(i2) <- (-. s2) *. cc1 -. r1 *. ss1
        and none_ = px..(i3) <- s3 *. cc3 +. r2 *. ss3
        and none_ = py..(i3) <- r2 *. cc3 -. s3 *. ss3 in
        loop1 (i0 + id) (i1 + id) (i2 + id) (i3 + id) id
      withtype {i0:nat}{i1:int}{i2:int}{i3:int | i0 <= i1 <= i2 <= i3}{id:nat}
               int(i0) -> int(i1) -> int(i2) -> int(i3) -> int(id) -> unit in
      let rec loop2 is id =
        if is >= n then () else begin
          let i1 = is + n4 in
          let i2 = i1 + n4 in
          let i3 = i2 + n4 in
          loop1 is i1 i2 i3 id;
          loop2 (2 * id - n2 + j) (4 * id)
        end
      withtype {is:nat}{id:nat | id >= n2} int(is) -> int(id) -> unit in
      loop2 j (2 * n2)
    done; loop (n2 / 2) (n4 / 2)
    withtype int(n2) -> int -> unit in
    loop n (n / 4);

    let rec loop1 i0 i1 id =
      if gt_int i1 n then () else
      let r1 = px..(i0) in
      let none_ = px..(i0) <- r1 +. px..(i1)
      and none_ = px..(i1) <- r1 -. px..(i1) in
      let r1 = py..(i0) in
      let none_ = py..(i0) <- r1 +. py..(i1)
      and none_ = py..(i1) <- r1 -. py..(i1) in
      loop1 (i0 + id) (i1 + id) id
    withtype {i0:nat}{i1:int | i0 <= i1} int(i0) -> int(i1) -> {id:nat} int(id) -> unit in
    let rec loop2 is id =
      if is >= n then () else begin
        loop1 is (is + 1) id;
        loop2 (2 * id - 1) (4 * id)
      end
    withtype {is:nat}{id:nat | id >= 4} int(is) -> int(id) -> unit in
    loop2 1 4;

    let rec loop1 j k =
      if ge_int k j then j + k else loop1 (j - k) (k / 2)
    withtype {j:nat}{k:nat | k <= n / 2} int(j) -> int(k) -> [i:nat | i <= n] int(i) in
    let rec loop2 i j =
      if i >= n then () else begin
        if ge_int i j then () else begin
          let xt = px..(j) in px..(j) <- px..(i); Array.get px i <- xt;
          let xt = Array.get py j in Array.get py j <- Array.get py i; Array.get py i <- xt;
        end;
        loop2 (i + 1) (loop1 j (n / 2))
      end
    withtype {i:nat} int(i) -> {j:nat | j <= n} int(j) -> unit in
    loop2 1 1; n
withtype float vect(n+1) -> float vect(n+1) -> int(n) -> int(n)
;;

let fabs r = if r >. 0.0 then r else (-. r)
;;

let ffttest np =
  let none_ = print_int np and none_ = print_string "... " in
  let enp = float_of_int np and n2 = np / 2 in
  let npm = n2 - 1
  and pxr = make_vect (np+1) 0.0
  and pxi = make_vect (np+1) 0.0
  and t = pi /. enp in
  let none_ = Array.get pxr 1 <- (enp -. 1.0) *. 0.5
  and none_ = Array.get pxi 1 <- 0.0
  and none_ = pxr..(n2+1) <- (-. 0.5)
  and none_ = pxi..(n2+1) <- 0.0 in
  for i = 1 to npm do
    let j = np - i in
    let none_ = pxr..(i+1) <- (-. 0.5) and none_ = pxr..(j+1) <- (-. 0.5) in
    let z = t *. (float_of_int i) in
    let y = 0.5 *. cos(z) /. sin(z) in
    pxi..(i+1) <-  (-. y); pxi..(j+1) <- y
  done;
  fft pxr pxi np;
  let rec loop i zr zi kr ki =
    if ge_int i np then (zr, zi) else
    let a = fabs(pxr..(i+1) -. (float_of_int i)) in
    let (zr, kr) = if zr <. a then (a, i) else (zr, kr) in
    let a = fabs(pxi..(i+1)) in
    let (zi, ki) = if zi <. a then (a, i) else (zi, ki) in
    loop (i+1) zr zi kr ki
  withtype {i:nat} int(i)  -> float -> float -> int -> int -> float * float in
  let (zr, zi) = loop 0 0.0 0.0 0 0 in
  let zm = if fabs zr <. fabs zi then zi else zr
  in print_float zm; print_newline ()
withtype {np:int | np >= 2} int(np) -> unit
;;

let rec loop_np i np =
  if i > 16 then () else begin ffttest np; loop_np (i + 1) (np * 2) end
withtype int -> {np:int | np >= 2} int(np) -> unit
;;

let doit () = loop_np 4 16;;
*)
