(*
 * by: Dave Edelblute, edelblut@cod.nosc.mil, 05 Jan 1993
 * Modified: R. Mayer to work with hist benchmark routines.
 * Translated from C to DML : Hongwei Xi, September 23, 2001
 *)

val PI = 3.14159265358979323846
val TWO_PI = 2.0 *. PI

let{n:int | n >= 2} fft px py n = (* n must be a power of 2! *)
  let rec{n2:nat} loop n2 n4 =
    if le_int n2 2 then () else (* the case n2 = 2 is treated below *)
    let e = TWO_PI /. (float_of_int n2) in let e3 = 3.0 *. e in
    let a = ref 0.0 and a3 = ref 0.0 in
    for j = 1 to n4 do
      let cc1 = cos !a and ss1 = sin !a and cc3 = cos !a3 and ss3 = sin !a3 in
      let _ = a := !a +. e and _ = a3 := !a3 +. e3 in
      let rec loop1 i0 i1 i2 i3 id =
        if gt_int i3 n then () else (* out_of_bounds *)
        let r1 = px..(i0) -. px..(i2)
        and _ = px..(i0) <- px..(i0) +. px..(i2)
        and r2 = px..(i1) -. px..(i3)
        and _ = px..(i1) <- px..(i1) +. px..(i3)
        and s1 = py..(i0) -. py..(i2)
        and _ = py..(i0) <- py..(i0) +. py..(i2)
        and s2 = py..(i1) -. py..(i3)
        and _ = py..(i1) <- py..(i1) +. py..(i3) in
        let s3 = r1 -. s2 and r1 = r1 +. s2
        and s2 = r2 -. s1 and r2 = r2 +. s1 in
        let _ = px..(i2) <- r1 *. cc1 -. s2 *. ss1
        and _ = py..(i2) <- (-. s2) *. cc1 -. r1 *. ss1
        and _ = px..(i3) <- s3 *. cc3 +. r2 *. ss3
        and _ = py..(i3) <- r2 *. cc3 -. s3 *. ss3 in
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
      let _ = px..(i0) <- r1 +. px..(i1)
      and _ = px..(i1) <- r1 -. px..(i1) in
      let r1 = py..(i0) in
      let _ = py..(i0) <- r1 +. py..(i1)
      and _ = py..(i1) <- r1 -. py..(i1) in
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
          let xt = px..(j) in px..(j) <- px..(i); px..(i) <- xt;
          let xt = py..(j) in py..(j) <- py..(i); py..(i) <- xt;
        end;
        loop2 (i + 1) (loop1 j (n / 2))
      end
    withtype {i:nat} int(i) -> {j:nat | j <= n} int(j) -> unit in
    loop2 1 1; n
withtype {n:int | n >= 0}
         float vect(n+1) * float vect(n+1) * int(n) -> in

(*
let fabs r = if r >. 0.0 then r else (-. r)

let ffttest np =
  let _ = print_int np and _ = print_string "... " in
  let enp = float_of_int np and n2 = np / 2 in
  let npm = n2 - 1
  and pxr = make_vect (np+1) 0.0
  and pxi = make_vect (np+1) 0.0
  and t = PI /. enp in
  let _ = pxr..(1) <- (enp -. 1.0) *. 0.5
  and _ = pxi..(1) <- 0.0
  and _ = pxr..(n2+1) <- (-. 0.5)
  and _ = pxi..(n2+1) <- 0.0 in
  for i = 1 to npm do
    let j = np - i in
    let _ = pxr..(i+1) <- (-. 0.5) and _ = pxr..(j+1) <- (-. 0.5) in
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
  withtype {i:nat} int(i) -> float -> float -> int -> int -> float * float in
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