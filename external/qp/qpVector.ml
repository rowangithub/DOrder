type 'a vector = {
  mutable a : 'a array;
  mutable u : int;
  d : 'a;
}

let obj_count = ref 0

let make size value =
  let res = {a = Array.make (if size < 16 then 16 else size) value; u = size; d = value} in
    (* obj_count := !obj_count + 1;
       Gc.finalise (fun _ -> obj_count := !obj_count - 1) res; *)
    res

let print_stats _ =
  Printf.printf "unfinalized vectors: %d" !obj_count;
  print_newline ()

let set v i vl =
  v.a.(i) <- vl

let get v i = v.a.(i)

let length v = v.u

let resize v n =
  v.u <- n;
  let len = Array.length v.a in
    if n > len then
      let newlen = if n > 2 * len then n else 2 * len in
      let old = v.a in
	v.a <- (Array.make newlen v.d);
	Array.blit old 0 v.a 0 len
	  
let push v vl =
  let i = v.u in
    resize v (i + 1);
    set v i vl
      
let pop v =
  v.u <- v.u - 1

let back v = 
  v.a.(v.u - 1)

