(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
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

module F = Format

module StringMap = Map.Make(struct type t = string let compare = compare end)

module ComparablePath = struct
  type t = Path.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module PathMap = Map.Make(ComparablePath)


let str_contains s1 s2 =
	let re = Str.regexp_string s2 in
	try 
	   ignore (Str.search_forward re s1 0); 
	   true
	with Not_found -> false

let maybe_cons m xs = match m with
  | None -> xs
  | Some x -> x :: xs

let rec _fli f n b = function
  | [] -> b
  | x :: xs -> _fli f (n + 1) (f n b x) xs

let fold_lefti f b lst =
  _fli f 0 b lst

let rec map3 f xs ys zs = match (xs, ys, zs) with
  | ([], [], []) -> []
  | (x :: xs, y :: ys, z :: zs) -> f x y z :: map3 f xs ys zs
  | _ -> assert false

let zip_partition xs bs =
  let (xbs,xbs') = List.partition snd (List.combine xs bs) in
  (List.map fst xbs, List.map fst xbs')

let flap f xs = 
  List.flatten (List.map f xs)

let flap2 f xs ys = 
  List.flatten (List.map2 f xs ys)

let flap3 f xs ys zs =
  List.flatten (map3 f xs ys zs)

let rec expand f xs ys =
  match xs with
  | [] -> ys
  | x::xs ->
      let (xs',ys') = f x in
      expand f (List.rev_append xs' xs) (List.rev_append ys' ys)

let do_catch s f x =
  try f x with ex -> 
     (
			(*if (String.compare s "" = 0) then ()
			else Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex);*) raise ex) 

let do_catch_ret s f x y = 
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); y) 
       
let do_memo t f arg key =
  try Hashtbl.find t key with Not_found ->
    let rv = f arg in
    let _ = Hashtbl.replace t key rv in
    rv

let rec map_partial f = function 
  | [] -> [] | x::xs -> 
      (match f x with 
       | None -> map_partial f xs 
       | Some y -> y::(map_partial f xs))

let remove_duplicates l =
  	let remove_elt e l =
  		let rec go l acc = match l with
    		| [] -> List.rev acc
    		| x::xs when e = x -> go xs acc
    		| x::xs -> go xs (x::acc)
  		in go l []
		in
		let rec go l acc = match l with
    	| [] -> List.rev acc
    	| x :: xs -> go (remove_elt x xs) (x::acc)
  	in go l []
		
let remove_customized_duplicates f l =
  	let remove_elt e l =
  		let rec go l acc = match l with
    		| [] -> List.rev acc
    		| x::xs when f e x -> go xs acc
    		| x::xs -> go xs (x::acc)
  		in go l []
		in
		let rec go l acc = match l with
    	| [] -> List.rev acc
    	| x :: xs -> go (remove_elt x xs) (x::acc)
  	in go l []

let mapfold f xs b = 
  let rec _mf ys b = function
    | [] -> 
        (List.rev ys,b)
    | x::xs -> 
        let (y',b') = f x b in
        _mf (y'::ys) b' xs in
  _mf [] b xs
	
let find_sharing xs eq = 
	if (List.length xs = 0) then []
	else if (List.length xs = 1) then List.hd xs 
	else 
		let x = List.hd xs in
		List.filter (fun e -> 
			List.for_all (fun x -> List.exists (
				fun e' -> eq e e') x) (List.tl xs)
			) x
			

(* extract k elements from a list *)			
let extract k list =
    let rec aux k acc emit = function
      | [] -> acc
      | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t
    in
    let emit x acc = x :: acc in
    aux k [] emit list	
		
(* find a sublist from positino b to e *)		
let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail						
			
(* find all digits of an integer *)
let digits n =
	let rec loop n acc =
	    if n = 0 then acc
	    else loop (n/10) (n mod 10::acc) in
	match n with
	| 0 -> [0]
	| _ -> loop n []
						
			
let hashkeys table = 
	Hashtbl.fold (fun k v res -> k::res) table []			

let incpp ir = 
  incr ir;!ir

let array_to_index_list a = 
  List.rev (snd 
    (Array.fold_left (fun (i,rv) v -> (i+1,(i,v)::rv)) (0,[]) a))

let pprint_list sepstr pp =
  (fun ppf -> Oprint.print_list pp
     (fun ppf -> F.fprintf ppf "%s@;<1 2>" sepstr) ppf)

let resl_opt f = function
  | Some o -> f o
  | None -> []

let resi_opt f = function
  | Some o -> f o
  | None -> ()

let opt_iter f l = 
  List.iter (resi_opt f) l

let add il i = il := i::!il
let addl il i = il := List.rev_append i !il

let same_type q p = (Types.TypeOps.equal q p)

let dummy () = Path.mk_ident ""

(****************************************************************)
(************* Output levels ************************************)
(****************************************************************)

(* verbosity levels by purpose *)
let ol_always = 0
let ol_solve_error = 1
let ol_warning = 1
let ol_solve_master = 2
let ol_solve_stats = 2
let ol_timing = 2
let ol_default = 2
let ol_normalized = 3
let ol_dquals = 4 
let ol_unique_names = 9
let ol_solve = 10 
let ol_refine = 11 
let ol_scc = 12 

let verbose_level = ref ol_default
let null_formatter = F.make_formatter (fun a b c -> ()) ignore
let nprintf a = F.fprintf null_formatter a
let ck_olev l = l <= !verbose_level

let cprintf l = if ck_olev l then F.printf else nprintf
let ecprintf l = if ck_olev l then F.eprintf else nprintf

let fcprintf ppf l = if ck_olev l then F.fprintf ppf else nprintf

let icprintf printer l ppf = if ck_olev l then printer ppf else printer null_formatter

let cprintln l s = if ck_olev l then Printf.ksprintf (F.printf "@[%s@\n@]") s else nprintf

let path_name () = if ck_olev ol_unique_names then Path.unique_name else Path.name

(****************************************************************)
(************* SCC Ranking **************************************)
(****************************************************************)

module Int : Graph.Sig.COMPARABLE with type t = int * string =
struct
   type t = int * string 
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

module G = Graph.Imperative.Digraph.Concrete(Int)

module SCC = Graph.Components.Make(G)    

 (* Use of Graphviz *)

let io_to_string = function 
  | Some i -> string_of_int i 
  | None -> "*"

let xs_to_string f xs =
  "["^(String.concat "," (List.map f xs))^"]"

module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name (i,s) = Printf.sprintf "V_%d_%s" i s 
   let vertex_attributes v = [`Label (vertex_name v)]
   let default_edge_attributes g = []
   let edge_attributes e = []
   let get_subgraph v = None
end

module Dot = Graph.Graphviz.Dot(DotGraph) 

let dump_graph g = 
  let oc = open_out "constraints.dot" in
  Dot.output_graph oc g;
  close_out oc

 
(* Given list [(u,v)] returns a numbering [(ui,ri)] s.t. 
 * 1. if ui,uj in same SCC then ri = rj
 * 2. if ui -> uj then ui >= uj *)
let scc_rank f ijs = 
  let g = G.create () in
  let _ = List.iter (fun (i,j) -> G.add_edge g (i,(f i)) (j,(f j))) ijs in
  let _ = dump_graph g in
  let a = SCC.scc_array g in
  let _ = cprintf ol_scc "@[dep@ graph:@ vertices@ =@ @ %d,@ sccs@ =@ %d@ @\n@]" 
          (G.nb_vertex g) (Array.length a);
          cprintf ol_scc "@[scc@ sizes:@\n@]";
          let int_s_to_string (i,s) = Printf.sprintf "(%d,%s)" i s in
          Array.iteri 
          (fun i xs -> 
          cprintf ol_scc "@[%d@ :@ %s@ @\n@]" i (xs_to_string int_s_to_string xs)) a;
          cprintf ol_scc "@[@\n@]" in
  let sccs = array_to_index_list a in
  flap (fun (i,vs) -> List.map (fun (j,_) -> (j,i)) vs) sccs

(*
let g1 = [(1,2);(2,3);(3,1);(2,4);(3,4);(4,5)];;
let g2 = [(0,1);(1,2);(2,0);(1,3);(4,3);
          (5,6);(5,7);(6,9);(7,9);(7,8);(8,5)];;
let g3 = (6,2)::g2;;
let g4 = (2,6)::g2;;
  
let n1 = make_scc_num g1 ;;
let n2 = make_scc_num g2 ;;
let n3 = make_scc_num g3 ;;
let n4 = make_scc_num g4 ;; *)


let asserts s b = 
  try assert b with ex -> 
    Printf.printf "Common.asserts failure: %s " s; raise ex

let append_to_file f s = 
  let oc = Unix.openfile f [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 420  in
  ignore (Unix.write oc s 0 ((String.length s)-1) ); 
  Unix.close oc

let write_to_file f s =
  let oc = open_out f in
  output_string oc s; 
  close_out oc



(**************************************************************************)
(****************** Type Specific to_string routines **********************)
(**************************************************************************)

let fsprintf f p = 
  Format.fprintf Format.str_formatter "@[%a@]" f p;
  Format.flush_str_formatter ()
(*
let pred_to_string p = 
  fsprintf Predicate.pprint p
*)

(*************************************************************************)

let map_cnt f m =
  let cnt a b n = n + 1 in
    f cnt m 0

let set_cnt f s =
   List.length (f s)
