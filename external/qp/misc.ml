(*
 * Copyright © 1990-2002 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

 (*
 * This file is borrowed from the BLAST Project.
 *)

(**
 * This module provides some miscellaneous useful helper functions.
 *)

exception Okfailure of string

let failokwith s = raise (Okfailure s)

let error s =
  Printf.printf "Fatal Error: %s \n" s; exit 0

let do_catch s f x =
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s" s (Printexc.to_string ex); raise ex) 

let do_catch_ret s f x y = 
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s" s (Printexc.to_string ex); y) 

let map_pair f (x,y) = (f x, f y)

let id x = x

let inc x = 
  x := !x + 1

let swap (x,y) = (y,x)

let fst3 (x,y,z) = x
let snd3 (x,y,z) = y
let thd3 (x,y,z) = z

  
let flap f xs = List.flatten (List.map f xs)

let findi f xs = 
  let rec _f i l = 
    match l with [] -> raise Not_found 
  | h::t -> if f h then i else _f (i+1) t in
  _f 0 xs

(* Poor man's set operations (in terms of lists) *)

(* add an element to a list if it's not already in there *)
let add lst elem = if List.mem elem lst then lst else elem::lst

(* union two lists representing sets *)
let union l1 l2 = List.fold_left add l2 l1

let last_two l = 
   match (List.rev l) with h1::h2::_ -> (h2,h1) 
   | _ -> failwith "last_two" 



(* let compact l = union l [] *)

(* set difference using lists to represent sets *)

let difference l1 l2 = List.filter (function elem -> not(List.mem elem l2)) l1
let proj_difference_l l1 l2 = List.filter (function (x,y) -> not (List.mem x l2)) l1
let proj_difference_r l1 l2 = let l2' = List.map fst l2 in difference l1 l2'

let list_remove l e = List.filter (fun x -> not (compare x e = 0)) l

let list_filter_unique f l = 
  match List.filter f l with
  [a] -> a
  | [] -> failwith "filter_unique : empty!"
  | _ -> failwith "filter_unique : multiple!"

let intersection l1 l2 = difference l1 (difference l1 l2)


(* set containment using lists *)
let rec subsetOf l1 l2 =
  match l1 with
    [] -> true
  | a::rest -> (List.mem a l2) && (subsetOf rest l2)

(* Cartesian product *)
let cartesianProduct ll =
  let  cartprodIt xs ys = List.fold_right
      (function x -> function pairs ->
        List.fold_right (function y -> function l ->
          (x::y):: l) ys pairs)
      xs []
  in
  List.fold_right cartprodIt ll [[]]


(* given a list of strings, produce a single string that is the
   comma-separated list of the original strings *)
let strList l = 
  String.concat ", " l

let mapcat f xs = 
  strList (List.map f xs)

(* returns the to_string function corresponding to the given
   pretty-printer *)
let to_string_from_printer printer =
  function data ->
    printer Format.str_formatter data ;
    Format.flush_str_formatter ()

(* given a printer for a type t, returns a printer for lists of elements of
   type t *)
let list_printer_from_printer printer fmt list =
  Format.fprintf fmt "@[[@[" ;
  begin
    match list with
        [] ->
          ()
      | head::tail ->
          printer fmt head ;
          List.iter (function e -> Format.fprintf fmt ";@ " ;
                                   printer fmt e)
                    tail
  end ;
  Format.fprintf fmt "@]]@]"

(* returns true if the character is a digit [0-9] *)
let is_digit c =
	match c with
		'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
	| _ -> false


let list_iteri f xs = 
  ignore(List.fold_left (fun i x -> f i x; i+1) 0 xs)

(*  
let iteri f xs =
  let rec _m i l = 
    match l with [] -> ()
    | h::t -> ((f i h);(_m (i+1) t)) in
  _m 0 xs
*)

let mapi f xs = 
  let rec _m i l = 
    match l with [] -> []
    | h::t -> (f i h)::(_m (i+1) t) in
  _m 0 xs

let map_exn ex f l = 
  let rec _map _l = 
    match _l with
      [] -> []
    | h::t -> try (f h)::(_map t) with ex -> (_map t)
  in
  _map l

let list_count x = 
  List.fold_left (fun c y -> c + (if x = y then 1 else 0)) 0 

let rec map_partial f l = 
  match l with [] -> [] | h::t -> 
    (match f h with
      Some x -> x::(map_partial f t)
    | None -> (map_partial f t))

let hashtbl_map_partial f t = 
  let res = ref [] in
  let _iter a b = 
    match f a b with
	None -> ()
      | Some(r) -> res := r::!res
  in
    Hashtbl.iter _iter t;
    !res

let filter_cut f l = List.partition f l 
 
(* returns the truncated list containing the first n 
 * elements Raise Failure "nth" if list is too short *)
let truncateList lst n =
  let rec _truncateWorker l rest len =
    if len = 0 then rest else 
    match l with
      [] -> failwith "truncateList: list too short"
    | head::tail -> _truncateWorker tail (rest @[head]) (len - 1)
  in
  _truncateWorker lst [] n


let chop_list f l = 
  let rec search st fin = 
    match fin with
	[] -> (st,fin)
      | h::tl -> if f h then (st,fin) else search (st@[h]) tl
  in
    search [] l

(*
let replace_string old_s new_s s = 
  let reg = Str.regexp old_s in
  Str.global_replace reg new_s s



(* chop s chopper returns ([x;y;z...]) if s = x.chopper.y.chopper ...*)
let chop s chopper = 
  let reg = Str.regexp chopper in
    Str.split reg s  


(* like chop only the chop is by chop+ *)
let chop_star  chopper s = 
  let reg = Str.regexp (Printf.sprintf "[%s+]" chopper) in
    Str.split reg s

let bounded_chop s chopper i = 
  let reg = Str.regexp chopper in
    Str.bounded_split reg s i 
      
let is_prefix p s = 
  let reg = Str.regexp p in
    Str.string_match reg s 0

let is_suffix suffix s = 
  let reg = Str.regexp suffix in
  let l = String.length suffix in
  let sl = String.length s in
  (sl - l >= 0) && Str.string_match reg s (sl - l)
 
let is_substring s subs = 
  let reg = Str.regexp subs in
    try 
      let _ = Str.search_forward reg s 0
      in true
    with
	Not_found -> false

let substitute_substrings s_s'_list one_string =
  let dosub (s,s') strn =      
    let reg = Str.regexp s in
    Str.global_replace reg s' strn
  in 
  List.fold_right dosub s_s'_list one_string
  
let k_suffix k s = 
  let s_len = String.length s in
  let rem_len = s_len - k in 
    if rem_len < 0 then failwith "bad call to k_suffix"
    else String.sub s k rem_len
      


(*  chop_after_prefix p s = if s = p.s' then s' *)
let chop_after_prefix prefix s =
  let reg = Str.regexp prefix in
    (*if (Str.string_match reg s 0) then *)
    (List.hd(Str.bounded_split reg s 1))

let chop_before_suffix suffix s =
  let reg = Str.regexp suffix in
  try 
    let pos = Str.search_backward reg s (String.length s -1) in
    String.sub s 0 pos
  with Not_found -> s

(* insert_before c s ins = s1 ins c s2 where s = s1 c s2 else s *)

let insert_before c s ins =
  let l = bounded_chop s c 2 in
    match l with
	[h] -> s
      | [s1;s2] -> Printf.sprintf "%s%s%s%s" s1 ins c s2
      | _ -> failwith "This cannot happen!"

let words_of_string string = 
  let reg = Str.regexp " +" in
  let l = Str.split reg string in
    List.filter (fun x -> not ((x = "") || (x = " "))) l

let grep_file sourcefile grepfile =
  let wordtable = Hashtbl.create 101 in
  let interesting_words = List.flatten (List.map words_of_string (string_list_of_file grepfile)) in
  let _ = List.iter (fun x -> Hashtbl.add wordtable x true) interesting_words in
  let lines_of_file = List.map words_of_string (string_list_of_file sourcefile) in
    List.filter (fun x -> List.exists (Hashtbl.mem wordtable) x) lines_of_file
 
*)

(** unzip  a,b-list returns a list, b list *)
  
let unzip l =
  let rec ac l l1 l2 =
    match l with
        (a,b)::tl -> ac tl (a::l1) (b::l2)
      | [] -> (List.rev l1, List.rev l2)
  in
    ac l [] []


(** hashtbl_keys tbl returns the list of keys that have bindings in the table -- assumes that every key has
  a unique binding *)

exception Hashtbl_exists


let hashtbl_exists f t =
  let _iter a b = 
    if f a b then raise Hashtbl_exists
  in
    try
      Hashtbl.iter _iter t;
      false
    with Hashtbl_exists -> true

let hashtbl_exists_key f t =
  let _iter a b = 
    if f a then raise Hashtbl_exists
  in
    try
      Hashtbl.iter _iter t;
      false
    with Hashtbl_exists -> true

let hashtbl_keys t =
  Hashtbl.fold (fun x y l -> x::l) t []
 
let hashtbl_data t = 
  Hashtbl.fold (fun x y l -> y::l) t []
    
let hashtbl_to_list t = 
   Hashtbl.fold (fun x y l -> (x,y)::l) t []

let hashtbl_of_list size l = 
   let t = Hashtbl.create size in
   List.iter (fun x -> Hashtbl.replace t x true) l;
   t
(** the hasthbl is key -> elem_list *)
let hashtbl_update table key element = 
  let ol = try Hashtbl.find table key with Not_found -> [] 
  in
    Hashtbl.replace table key (element::ol)
            
let hashtbl_check_update table key element = 
  let ol = try Hashtbl.find table key with Not_found -> [] 
  in
  let rv = not (List.mem element ol) in
    if rv then Hashtbl.replace table key (element::ol);
    rv

let hashtbl_delete table key element = 
  let ol = try Hashtbl.find table key with Not_found -> [] 
  in
    Hashtbl.replace table key (list_remove ol element)

(* like hashtbl_check_update -- only instead of ensuring that elt not in table.key
   we check that forall elt' in table.key not (f elt elt')
   i.e. not (exists elt'. f elt elt') *)
let hashtbl_fun_check_update f table key element =
  let ol = try Hashtbl.find table key with Not_found -> [] 
  in
  let rv = not (List.exists (f element) ol) in
    if rv then Hashtbl.replace table key (element::ol);
    rv

  
(* Hashtable union: hash_union t1 t2 adds all the data of t2 to t1 *)

let hashtbl_addtable t1 t2 = 
  let f_add key data = Hashtbl.add t1 key data in
    Hashtbl.iter f_add t2

(* search_list f l walks through l and finds the first guy st f(x) is some..., 
   it returns that some *)
let search_list f list = 
  let rec search l = 
    match l with 
	[] -> None
      | h::t -> 
	  begin
	    match f h with 
		Some(s) -> Some(s)
	      | None -> search t
	  end
  in
    search list

let cons_if hd tl =
  if not (List.mem hd tl) then hd::tl else tl
      
let rec list_gather_option l = 
  match l with
      [] -> []
    | Some(a)::tl -> a::(list_gather_option tl)
    | None::tl -> (list_gather_option tl)

let rec list_add l =
  let add x y = x + y in
    List.fold_right add l 0

let string_option_to_string so = 
  match so with
    Some(s) -> s
  | None -> "NONE"

let simplify_boolean_unroll l op base =
  List.fold_left (fun a b -> "( "^op^" "^b^" "^a^")") base l
	
let list_max l =
  match l with [] -> failwith "list_max: empty_list" 
  | h::t -> List.fold_left max h t

let write_to_file fname _string =
  let oc = open_out fname in
  output_string oc _string; 
  close_out oc

let write_list_to_file fname string_list =
  let oc = open_out fname in
    List.iter (output_string oc) string_list;
    close_out oc

(*
let append_counter_table = Hashtbl.create 31
let append_to_file fname s = 
   let oc = Unix.openfile fname  [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 420  in
   ignore (Unix.write oc s 0 ((String.length s)-1) ); 
   Unix.close oc
*)
let chop_last l = 
  if l = [] then [] else (List.rev (List.tl (List.rev l)))

let rec make_all_pairs l = 
  match l with
      [] -> []
    | h::tl -> (List.map (fun x -> (h,x)) tl)@(make_all_pairs tl)

let iter_cross f l1 l2 = 
  List.iter (fun i -> List.iter (f i) l2) l1

let cross_product l1 l2 = 
  flap (fun x -> List.map (fun y -> (x,y)) l2) l1

let array_reset arr resetval = 
  Array.fill arr 0 (Array.length arr) resetval

(* ambitious -- take a text file and return a list of lines that is the file *)
let string_list_of_file fname = 
  let ic  = open_in fname in
  let doneflag = ref false in
  let listoflines = ref [] in
    while not (!doneflag) do
      try
	listoflines := (input_line ic)::(!listoflines)
      with
	  End_of_file -> doneflag := true
    done;
    close_in ic;
  List.rev !listoflines


(* source file is some random text file and grepfile is a list of interesting 
words *)

let rec string_list_cat sep l = 
  match l with 
      [] -> "" 
    | [h] -> h
    | h::tl -> h^sep^(string_list_cat sep tl)

 

let run_on_table f tab = 
  let reslist_ref = ref [] in
    Hashtbl.iter (fun x y -> reslist_ref := (f x y)::(!reslist_ref)) tab;
    !reslist_ref
      
let list_last l = 
  try List.hd (List.rev l) with _ -> failwith "list_last on Empty list!"
 
let bool_to_string b = 
  match b with
      true -> "true"
    | false -> "false"

(** unzip  a,b-list returns a list, b list *)

let unzip l =
  let rec ac l l1 l2 = 
    match l with
	(a,b)::tl -> ac tl (a::l1) (b::l2)
      | [] -> (List.rev l1, List.rev l2)
  in
    ac l [] []

(** l is a list of even length -- this returns a pair of lists -- the even elts and the odd elts *)
let unzip_joined l = 
  let rec _uj l l1 l2 = 
    match l with
	[] -> (l1,l2)
      | h1::h2::t -> _uj t (h1::l1) (h2::l2)
      | [h1] -> (h1::l1,l2)
  in
  let (r1,r2) = _uj l [] [] in
    (List.rev r1,List.rev r2)

(** hashtbl_keys tbl returns the list of keys that have bindings in the table -- assumes that every key has 
  a unique binding *)

let hashtbl_keys t = 
  Hashtbl.fold (fun x y l -> x::l) t []
    
(** hashtbl_map tbl f returns the list f a b where the bindings in the table are a -> b *)
let hashtbl_map tbl f = 
  let keys = hashtbl_keys tbl in
    List.map (fun x -> f x (Hashtbl.find x)) keys

(** hashtbl_filter f tbl returns the list a, s.t. f a b = true, where a -> b in tbl *)
let hashtbl_filter f tbl =
  let rv = ref [] in
  let _iter a b = if f a b then rv := a::!rv in
    Hashtbl.iter _iter tbl;
    !rv
    
let hashtbl_filter_tbl f tbl = 
  let del (key,data) = if not (f key data) then Hashtbl.remove tbl key in
  let kd_list = hashtbl_to_list tbl in
  List.iter del kd_list
  
  
(** compact l -> l' where l' is the list l without redundancies *)
let compact l = 
  let _t = Hashtbl.create (List.length l) in
  let _ = List.iter (fun x -> Hashtbl.replace _t x true) l in
  hashtbl_keys _t

let bi_sort (x,y) = 
  if compare x y = 1 then (y,x) else (x,y)

let sort_and_compact ls =
  let rec _sorted_compact l = 
    match l with
	h1::h2::tl ->
	  let rest = _sorted_compact (h2::tl) in
	    if h1 = h2 then rest else h1::rest
      | tl -> tl
  in
    _sorted_compact (List.sort compare ls)
      
let sorted_subsetOf l l' =
  let rec _ss l1 l2 =
    match (l1,l2) with
	([],_) -> true
      | ((h1::t1),[]) -> false
      | ((h1::t1),(h2::t2)) -> if h1 = h2 then _ss t1 t2 else _ss l1 t2
  in
    _ss (sort_and_compact l) (sort_and_compact l')
  
(** assoc_list (  *)
let delete_assoc_list l1 l2 =
  let rec sf l1 l2 res = 
    match (l1,l2) with 
      ([],_) | (_,[]) -> (List.rev res)@l1 
    | (((_,i1) as h1)::t1,h2::t2) -> 
        if i1 < h2 then sf t1 l2 (h1::res)
        else if i1 > h2 then sf l1 t2 res
        else sf t1 t2 res in
  sf (List.sort (fun (_,x) (_,y) -> compare x y) l1) (List.sort compare l2) []


  (* compute the power set of a given set of elements represented as a 
     list.  the presence of an element e in a subset is represented as 
     (true, e), and the absence is represented as (false, e). 
  *)
  let powerSet l = 
(*
    let rec powAux currSet elems =
      match elems with
          [] -> [currSet]
        | head::tail ->
            (powAux ((true,head)::currSet) tail)@
            (powAux ((false,head)::currSet) tail) in
      powAux [] l
*)
  (* Generate all the (exponentially many) minterms to check. 
   * We are given l, a list of absPredExpressions. For example, l might
   * be [a;b;c]. We are supposed to generate:
   * 	a, ~a, b, ~b, c, ~c
   * 	ab, a~b, ~a~b, ~ab, ac, a~c, ~ac, ~a~c, bc, b~c ...
   * 	abc, ab~c, a~bc, ...
   *
   * Through amazingly convoluted coding, this procedure uses an amount of
   * stack space that is LINEAR in MaxCubeLen (which is <= l).
   * Heap space is exponential, but so is the answer. *)
  match l with [] -> [[]] 
  | _ ->
  (* what is the maximum cube length for one of our answers? *)
    let maxCubeLen = List.length l
    in 
  (* we'll store our answers here *)
    let answer_list = ref [] in 
    
  (* we need random access to the input list of predicates, so we convert
     * it to an array *)
    let l_array = Array.of_list l in 
    let l_len = maxCubeLen - 1 in 
    
(*[rupak]    for cubeLen = maxCubeLen downto 1 do *) let cubeLen = maxCubeLen in (*[rupak]*)
    (* now we want to spit out all minterm-lists of length cubeLen *)
    (* so we will consider all subsets of l of length cubeLen *)
    (* inThisSubset is an array of indices into l_array *)
      let inThisSubset = Array.make cubeLen (-1) in
    (* exampe: if inThisSubset.(0) = 3 and inThisSubset.(1) = 5 then
       * this subset consists of elements 3 and 5 *)
      
    (* we also want to know if we are including the input term or its
       * negation *)
      let polarity = Array.make cubeLen 0 in 
      
    (* Here comes the black magic. Normally to enumerate all subsets of
       * length three you would use three nested for-loops. To enumerate
       * all subsets of length cubeLen we need cubeLen nested for loops.
       * We will build our loop nest up dynamically using function pointers.
       *
       * To avoid considering both 3,5 and 5,3 as subsets of length 2 we will
       * only consider subsets that have their l-indices in ascending order.
       *)

    (* "iter" is one such for loop. 
       * "index" tells us which element of inThisSubset and polarity we are
       * setting. "last_value" helps us stay in ascending order.
       * "continuation" is what to call when we are done. We call it with
       * our current value as its only argument. 
       *)
      let iter (index : int) (last_value : int) (continuation : int -> unit) =
	for i = last_value+1 to l_len do (* consider every element in l *)
	  inThisSubset.(index) <- i; (* it just became element 'index' 
			 		* of our subset *)
	  for pol = 0 to 1 do 	   (* include it once positively *)
	    polarity.(index) <- pol ; (* and once negatively *)
	  (continuation) i
	  done
	done
      in 
      (* "construct" is the inner-most loop. Given that previous iterations
       * have set up the inThisSubset and polarity arrays for us, construct
       * the associated cube. *)
      let construct _ = 
	let this_cube = ref [] in (* we'll build the cube here *)
	for gather = 0 to cubeLen-1 do (* pick out all elts in this subset *)
	  let elt = l_array.(inThisSubset.(gather)) in 
	  let elt' = if polarity.(gather) = 0 then 
	    (false,elt) else (true,elt) in 
	  this_cube := elt' :: !this_cube ; 
	done ; (* now that we have it, append it to the answer list *)
	answer_list := !this_cube :: !answer_list
      in 
      (* Now it's time to built up our loop nest. Recall that we'll have
       * cubeLen for loops. We'll store them in this array. The default
       * array element is "construct", which handles the innermost loop. *)
      let funPtrArray = Array.make cubeLen construct in
      (* NOTE: ocaml will not do what you want if you try to define these
       * guys like: "myfun := iter x y (!myfun)". So we need the array. *)
     
      (* Actually constuct the loop nest. Each iteration calls another
       * iteration. *)
    for i = 1 to cubeLen-1 do
      funPtrArray.(i) <- (fun last_val -> iter i last_val funPtrArray.(i-1)) 
    done ; 
    (* Here's the outermost loop: This one is an actual function call. *)
      iter 0 (-1) (funPtrArray.(cubeLen-1)) ;
    (*[rupak]done;[rupak]*)
    !answer_list


let power a b =
  if b>= 0 then
    begin
      if b = 0 then 1 else
      let e = ref b  in
      let p = ref a  in
      while !e > 1 do
	p := !p * !p ; e := !e / 2;
      done ;
      if b mod 2 = 0 then !p else !p * a
    end
  else invalid_arg "power: negative power"

let list_ordered_map f l = 
  let rec _acm _l =
    match _l with
	[] -> []
      | h::t -> let r = f h in r::(_acm t)
  in
    _acm l

let list_cut i l1 =
  let rec _lc i l1 l2 =
    (* assert (i >= List.length l1); *)
    if i = 0 then (List.hd l1, l2)
    else
      match l1 with
	| h::t ->
	    _lc (i-1) t (h::l2)
	| _ -> failwith "unhandled case :: list_cut @ misc.ml"
	    (*
      let h::t = l1 in
	_lc (i-1) t (h::l2)*)
  in
    _lc i l1 []

let list_cut' i l1 =
  let rec _lc i l1 l2 =
    (* assert (i >= List.length l1); *)
    if i = 0 then (List.tl l1, List.hd l1, l2)
    else 
      match l1 with
	| h::t ->
	    _lc (i-1) t (h::l2)
	| _ -> failwith "unhandled case :: list_cut' @ misc.ml"
(*      let h::t = l1 in
	_lc (i-1) t (h::l2)*)
  in
    _lc i l1 []

(* assumes that all the supplied integers are  >= 0 *)
let min (l: int list) = 
  let rec _min _l curr =
    match _l with 
	[] -> curr 
      | h::t -> 
	  _min t (if (curr = -1 or h < curr) then h else curr)
  in
    _min l (-1)

(** repeats f: unit - > unit i times *)
let rec repeat_fn f i = 
  if i = 0 then ()
  else (f (); repeat_fn f (i-1))

(** takes a dom_rel should be a p.o. I guess, and returns the maximal elts of list_l *)
(* as you can see, a most frivolous algorithm, but hey! *)
let maximal dom_rel list_l = 
  let stored_ll = list_l in
  let is_not_dominated l = not (List.exists (fun x -> dom_rel x l) stored_ll) in
    List.filter is_not_dominated list_l


(** binary_search f (lo,hi) returns  k where:
  * lo <= k <= hi, 
  * forall lo <= j < k: (f j = false)
  * k <> hi => (f k = true) *)

let binary_search f (lo,hi) =
  let rec _bs ((lo : int),hi) = 
    (assert (lo <= hi);
    if lo = hi then lo else
      let x = (lo + hi)/2 in
      if f x then _bs (lo,x) else _bs (x+1,hi)) in
  _bs (lo,hi) 



(* tmp *)




(* input: 'a array, f: 'a -> paren. Output: l,r : int -> int *)

let paren_match f arr =
  let n = Array.length arr in
  let left_array = Array.create n (-1) in
  let right_array = Array.create n (-1) in
  let pot = ref [] in
    for i = 0 to n-1 do
      match (f (arr.(i))) with
	| "(" -> (pot := i::!pot;Array.set left_array i i)
	| ")" -> 
	      (match !pot with
		| (tos::tl) ->
		    Array.set right_array tos i;
		    Array.set left_array i tos;
		    pot := tl
		| _ -> failwith "unhandled match case :: paren_match @ misc.ml")
	| _ ->  let tos = try List.hd !pot with _ -> -1 in 
                Array.set left_array i tos
    done;
  let l i = if i < 0 || i >= Array.length left_array then -1 else left_array.(i) in
  let r i = if (l i = -1) then -1 else right_array.(l i) in
    (l,r)


let gen_paren_match f arr =
  let pot = Hashtbl.create 37 in
  let _get k = try Hashtbl.find pot k with Not_found -> [] in
  let _set k l = Hashtbl.replace pot k l in
  let push k i = let l = _get k in _set k (i::l) in
  let pop k = match _get k with i::l -> (_set k l; i) | _ -> failwith "bad match" in
  try
    let n = Array.length arr in
    let left_array = Array.create n (-1) in
    let right_array = Array.create n (-1) in
      for i = 0 to n-1 do
        match f (arr.(i)) with
        | ("(",k) -> push k i
        | (")",k) -> 
            let i' = pop k in (Array.set right_array i' i;Array.set left_array i i')
        | _ -> ()
      done;
    let l i = if i < 0 || i >= Array.length left_array then (-1) else left_array.(i) in
    let r i = if i < 0 || i >= Array.length left_array then (-1) else right_array.(i) in
     Some (l,r)
  with _ -> None
   
let rec list_tabulate f start stop = 
  if start >= stop then []
  else 
    (f start)::(list_tabulate f (start+1) stop)

let rec _kcombine l = 
  (* each list in l must have the same length *)
  match l with
      [] -> []
    | _ -> 
	try
	  let l' = List.filter (fun l'' -> l'' <> []) l in
	  let prefix = List.map List.hd l' in
	  let suffix = List.map List.tl l' in
	    prefix @ (_kcombine suffix)
	with
	    _ -> failwith "Failure in kcombine"

let kcombine l = 
  let s = strList (List.map (fun l' -> string_of_int (List.length l')) l) in
    print_string ("Kcombine called with : "^s^" : \n");
    _kcombine l

let rec partial_combine l1 l2 =
  match (l1,l2) with
      (h1::t1,h2::t2) -> (h1,h2)::(partial_combine t1 t2)
    | _ -> []
	
(** make_list n : returns the list [1;2;...;n]  if n > 0 else [] *)
let make_list n = 
  let rec _ac i = 
    if i = 0 then []
    else i:: (_ac (i-1))
  in
    if (n < 0) then []
    else List.rev (_ac n)

exception Hashtbl_forall_fails 

let hashtbl_forall f tbl = 
  let check_fn x y = 
    if f x y then ()
    else raise Hashtbl_forall_fails
  in
    try
      Hashtbl.iter check_fn tbl;
      true
    with Hashtbl_forall_fails -> false



  
let cut_list_at_k l  k = 
  let rec _ac l1 l2 _k = 
    if _k <= 0 then (l1,l2)
    else 
      match l2 with
	  h::t -> _ac (h::l1) t (_k - 1)
	| [] -> failwith "Cannot cut at k: list too short!"
  in
    _ac [] l k


let rec get_first_array a f idx =
  if (idx < 0 || idx >= Array.length a) then None
  else if f (a.(idx)) then Some(idx) 
  else get_first_array a f (idx+1)
  
let rec get_first f l =
  match l with
      [] -> None
    | h::t -> 
	begin
	  match f h with
	      Some b -> Some b
	    | None -> get_first f t
	end

    
let rec get_first_suffix f l =
  match l with [] -> []
  | h::t -> if f h then l else (get_first_suffix f t)

let get_first_cont_suffix f l = 
  let fn = fun x -> not (f x) in
  let rec _gfcs l = 
  match get_first_suffix f l with [] -> []
  | h::t -> 
      begin 
        match get_first_suffix fn t with [] -> h::t
        | l' -> _gfcs l'
      end
  in 
  _gfcs l


        
let get_last f l = 
  get_first f (List.rev l)

let array_of_list2 l =
  Array.of_list (List.map (Array.of_list) l)

let array_filter f arr =
  let iptr = ref [] in
  let collect i a =
    if f a then 
      iptr := i::!iptr;
  in
    Array.iteri collect arr;
    List.rev !iptr

let array_filteri f arr = 
  let aptr = ref []  in
  let collect i a = if f i then aptr := a::!aptr;
  in
    Array.iteri collect arr;
    List.rev !aptr

let array_select2 arr l = 
  try
    List.map (List.map (fun i -> arr.(i))) l
  with _ -> failwith "array_select2 fails!"

let rec make_list_pairs l =
  match l with
      [] -> []
    | [h] -> []
    | a::b::t -> (a,b)::(make_list_pairs (b::t))

let rec group_pairwise l = 
  match l with
    | [] -> []
    | a::b::t -> (a,b)::(group_pairwise t)
    | _ -> failwith "unhandled match case :: group_pairwise @ misc.ml"
    
let rec hd_ll l  =
  match l with
      [] -> None
    | l1::t -> if l1 <> [] then Some(List.hd l1) else hd_ll t

let string_of_int_list il = strList (List.map string_of_int il) 


let boolean_list_leq l1 l2 = 
  let bleq b1 b2 = (not (b1)) || b2 in
    (List.length l1 = List.length l2)
    && (List.for_all2 bleq l1 l2)

let get_binary i n =
  let rec n_zeros n = if n<=0 then [] else 0::(n_zeros (n-1)) in
  let j = ref i in
  let bits = ref [] in
  while (!j >= 1) do
    let thisbit = !j land 1 in
    bits := thisbit :: !bits ;
    j := !j lsr 1;
  done ;
  if (List.length !bits < n) then (n_zeros (n - List.length !bits)) @ !bits
  else !bits


let ignore_list_iter f l = ignore (List.map f l)
  
(* as always l must be a bunch of 'a lists with the same length *) 
(* how would you prove that ... *)
(* TBD: "infer" this has type: ('a list ['n]) list -> 'b list ['n] *)
let rec map_transpose f l = 
try 
  if (List.hd l <> []) 
  then
    let l_hd = List.map List.hd l in
    let l_tl = List.map List.tl l in
      (f l_hd)::(map_transpose f l_tl)
  else []
with e -> failwith "Error in map_transpose!"

let queue_to_list q = 
   let rv_l = ref [] in
   let _f e = rv_l := e :: !rv_l in
     Queue.iter _f q;
     List.rev !rv_l

let option_to_list o = 
   match o with
       Some x -> [x]
     | None -> []

(* mask f l returns the list l' which has the element i of l if f.i is true *)	 
let list_mask f l =
  let rec gather offset l =
    match l with
	[] -> []
      | h::t ->
	 (*  let _ = Printf.printf "list_mask (i, f.i) = %d %b \n" offset (f offset) in *)
	  if f offset then h::(gather (offset+1) t)
	  else (gather (offset+1) t)
  in
    gather 0 l

      
let array_counti f arr =
  let ctr = ref 0 in
  let _iter i a = if f i a then ctr := !ctr + 1 in
    Array.iteri _iter arr;
    !ctr

    
let nontrivial_intersect f cmp l1' l2' = 
   let rec _check l1 l2 =
    match (l1,l2) with
	(h1::t1,h2::t2) ->
	  let s = cmp h1 h2 in
	    if s = 0 then 
	      if f h1 then true
	      else (_check t1 t2)
	    else
	      if s < 0 then _check t1 l2
	      else _check l1 t2
      | _ -> false
  in
    _check (List.sort cmp l1') (List.sort cmp l2')

let nonempty_intersect cmp l1 l2 = 
  nontrivial_intersect (fun _ -> true) cmp l1 l2

let sort_and_intersect l1 l2 = 
  let i_list_ref = ref [] in
  let rec _ints  _l1 _l2 = 
    match (_l1,_l2) with
    (h1::t1,h2::t2) ->
      let s = compare h1 h2 in
      if s = 0 then (i_list_ref := h1::!i_list_ref; _ints t1 t2) 
      else if s < 0 then _ints t1 _l2 else _ints _l1 t2
    | _ -> ()
  in 
  _ints (List.sort compare l1) (List.sort compare l2);
  !i_list_ref

  
let exists_file file_name =
  try
    let ch = open_in file_name in
  close_in ch;
  true
   with _ -> false

      
(* HACK to get around the lvals_type -- see AliasAnalyzer.get_lval_aliases_iter *)
let allLvals_t_c = 0
let scopeLvals_t_c = 1
let traceLvals_t_c = 2
		       
let strIntList l =
  strList (List.map string_of_int l)
(** what every model checker needs -- a fixpoint computation. 
 * computes next_fn^*(i) *)
(*let compute_fixpoint next_fn seed =
  let fp_table = Hashtbl.create 17 in    
  let rec proc i =
    if (Hashtbl.mem fp_table i) then ()
    else
      begin
        Hashtbl.replace fp_table i true;
        let next = next_fn i in
        List.iter proc next
      end
  in
  List.iter proc seed;
  hashtbl_keys fp_table
*)      

  
let compute_fixpoint_bounded depth next_fn seed = 
  let fp_table = Hashtbl.create 17 in
  let rec bfs d s = 
    let get_next e = 
        if Hashtbl.mem fp_table e  then []
        else (Hashtbl.replace fp_table e (depth-d);next_fn e) 
    in
    if (d = 0 || s = []) then ()
    else
        let succs = List.flatten (List.map get_next s) in
        bfs (d-1) succs
  in
  bfs depth seed;
  hashtbl_to_list fp_table
       
  let compute_fixpoint next_fn seed = compute_fixpoint_bounded (-1) next_fn seed   

  
let accumulate f binop seed arglist = 
  let folder _sum arg = binop _sum (f arg) in
  List.fold_left folder seed arglist

    
(* let rec list_n l n = 
  assert (n >= 0);
  if (l = [] || n < 1) then []
  else
    match l with
      | h::t -> h::(list_n t (n-1))
      | _ -> failwith "unhandled match case :: list_n @ misc.ml" *)

let list_n l n = 
  let rec _ln l n a = 
    if n < 1 then List.rev a else
      match l with [] -> List.rev a
      | h::t -> _ln t (n-1) (h::a) in
  _ln l n []

let array_to_list_n arr n = list_n (Array.to_list arr) n 

let toString_list toString l = strList (List.map toString l)

let rec list_min_combine l1 l2 = 
  match (l1,l2) with
  ([],_) -> []
  | (_,[]) -> []
  | (h1::t1,h2::t2) -> (h1,h2)::(list_min_combine t1 t2)

let rec repeat f i =  if i > 0 then (f (); repeat f (i-1))

let list_argmin f l = 
  let rec _ac (e,i) l = 
    match l with
      [] -> (e,i) 
    | h::t -> 
        let i' = f h in 
        let c' = if (i' < i) then (h,i') else (e,i) in
        _ac c' t
  in
  match l with h::t -> (_ac (h, f h) t) | [] -> failwith "list_argmin: empty list!"
       

(********** hashed intset **********)
  
module IntHash = Hashtbl.Make(struct type t = int let equal x y = (x = y) let hash = Hashtbl.hash end)

module IntMap = Map.Make(struct type t = int let compare = compare end)

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)


let hashints_check_union table key eltset =
  let oldset = try Hashtbl.find table key with Not_found -> IntSet.empty in
  let newset = IntSet.union oldset eltset in
  let is_new = IntSet.cardinal oldset <> IntSet.cardinal newset in
  Hashtbl.replace table key newset;
  is_new

  

 
let list_exists_cross2 f l1 l2 = 
  List.exists (fun e -> (List.exists (fun e' -> f e e') l2)) l1

  
let base_convert b n = 
  let rec _ac n = 
    if n = 0 then [] else
      let r = n mod b in
      r::(_ac (n/b))
  in
  List.rev (_ac n)

let ascii_string = "abcdefghijklmnopqrstuvwxyz"
let ascii_of_int i = 
  if (0<=i && i <=25) then String.make 1 (ascii_string.[i])
  else (failwith "ascii_of_int: bad argument")

let ascii_string_of_int n =
  let n_26 = base_convert 26 n in
  String.concat "" (List.map ascii_of_int n_26)

let rec list_duplicate l = 
  match l with 
  [] | [_] -> false
  | h1::h2::t -> 
      if (compare h1 h2 = 0) then true 
      else (list_duplicate (h2::t))



let rec range (i,j,step) = 
  if i >= j then [] else i::(range(i+step,j,step))


let filter_i f l = 
  let rec _ac i l' = 
    match l' with [] -> []
    | h::t -> if f h then i::(_ac (i+1) t) else (_ac (i+1) t)
  in 
  _ac 0 l

let rec clone x n = 
  if n <= 0 then [] else (x::(clone x (n-1)))

let do_memo t f arg key =
  try Hashtbl.find t key with Not_found ->
    let rv = f arg in
    let _ = Hashtbl.replace t key rv in
    rv

let assert_false s =
  Printf.printf "Misc.asserts failure: %s " s;
  assert false

let asserts s b = 
  try assert b with ex -> 
    Printf.printf "Misc.asserts failure: %s " s; raise ex
    
(*************************************************************)
(* Code to deal with timeouts  *)

exception TimeOutException
 (* The trouble with raising TimeOutException here is 
  * that if TimeOutException is caught at some random place (and absorbed),
  * we have lost the alarm. Moreover this exception can be 
  * raised at an awkward moment, leaving the internal state inconsistent. 
  * So we set a global bit that the model checker will check at the beginning 
  * of each loop. The model_check routine raises TimeOutException if this bit is set. 
  * This bit is reset by the catchers of TimeOutException *)

(* 
let sig_caught_bit = ref false 

let set_time_out_signal t =
  (if t >= 0 then
       begin 
	 Printf.printf "Setting signal for %d seconds \n" t ;
	 sig_caught_bit := false ;
	 ignore (Sys.signal
                   Sys.sigalrm
                   (Sys.Signal_handle
                      (fun i ->
                        output_string stdout "Caught exception sigalrm in handler!" ;
                        print_string "Caught exception sigalrm in handler!" ;
			sig_caught_bit := true ; 
                       
			 exit 1;
                      )) ) ;
	 ignore (Unix.alarm (t)) ;
       end )
let reset_time_out_signal () =
  Sys.set_signal Sys.sigalrm (Sys.Signal_ignore) ;
  sig_caught_bit := false


let check_time_out () =
  if (!sig_caught_bit) then (* Time out has occurred *)
    begin
      reset_time_out_signal () ; (* reset the timeout for next iteration *)
      Printf.printf "Time out!\n\n" ; 
      raise TimeOutException
    end
*)

