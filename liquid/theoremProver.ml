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

(* Common theorem prover interface *)
module P = Predicate
module C = Common
module Cl = Clflags

(********************************************************************************)
(************************** Rationalizing Division ******************************)
(********************************************************************************)

let rec fixdiv p = 
  let expr_isdiv = 
    function P.Binop(_, P.Div, _) -> true
      | _ -> false in 
  let pull_const =
    function P.PInt(i) -> i
      | _ -> 1 in
  let pull_divisor =
    function P.Binop(_, P.Div, d1) ->
      pull_const d1 
      | _ -> 1 in
  let rec apply_mult m e =
    match e with
        P.Binop(n, P.Div, P.PInt(d)) ->
          (*let _ = assert ((m/d) * d = m) in*)
            P.Binop(P.PInt(m/d), P.Times, n) 
      | P.Binop(e1, rel, e2) ->
          P.Binop(apply_mult m e1, rel, apply_mult m e2) 
      | P.PInt(i) -> P.PInt(i*m)
      | e -> P.Binop(P.PInt(m), P.Times, e)
  in
  let rec pred_isdiv = 
    function P.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
      | P.Iff (px, q) -> expr_isdiv px || pred_isdiv q
      | P.And(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.Or(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.True -> false
      | P.Not p -> pred_isdiv p 
			| P.Reach (d, u) -> false
			| P.Link (d, c, f, u, v) -> false
			| P.Forall (ps, p) -> pred_isdiv p 
			| P.Bool b -> false in
  let calc_cm e1 e2 =
    pull_divisor e1 * pull_divisor e2 in
  if pred_isdiv p then
     match p with
       P.Atom(e, r, e') -> 
         let m = calc_cm e e' in
         let e'' = P.Binop(e', P.Minus, P.PInt(1)) in
         let bound (e, r, e', e'') = 
           P.And(P.Atom(apply_mult m e, P.Gt, apply_mult m e''),
                 P.Atom(apply_mult m e, P.Le, apply_mult m e')) in
           (match (e, r, e') with
                (P.Var v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | (P.PInt v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | _ -> p) 
     | P.And(p1, p2) -> 
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.And(p1, p2)      
     | P.Or(p1, p2) ->
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.Or(p1, p2) 
     | P.Not p1 -> P.Not(fixdiv p1) 
		 | P.Forall (ps, p) -> P.Forall (ps, fixdiv p) 
     | p -> p
    else p

(********************************************************************************)
(*********************** Memo tables and Stats Counters  ************************)
(********************************************************************************)

let nb_push = ref 0
let nb_queries = ref 0
let nb_cache_miss = ref 0
let nb_qp_miss = ref 0
let qcachet: (string * string, bool) Hashtbl.t = Hashtbl.create 1009 

(********************************************************************************)
(************************************* API **************************************)
(********************************************************************************)

(* API *)
let print_stats () =
  TheoremProverZ3.Prover.print_stats ();
  C.cprintf C.ol_solve_stats 
    "@[Prover stats:@ %d@ pushes,@ %d@ queries,@ %d@ cache@ misses,@ %d@ qprover@ misses@\n@]" 
    !nb_push !nb_queries !nb_cache_miss !nb_qp_miss; flush stdout


(* API *)
let reset () =
  Hashtbl.clear qcachet; 
  nb_push  := 0;
  nb_queries := 0; 
  nb_cache_miss := 0;
  nb_qp_miss := 0

(*
let imp_to_string p q = 
  Printf.sprintf "%s => %s" (C.pred_to_string p) (C.pred_to_string q)
*)

(* API *)
let implies p = 
  let _ = incr nb_push in
  let p = fixdiv p in
  (* let check_qp = 
    if !Cl.use_qprover 
    then Bstats.time "QP implies" TheoremProverQprover.implies p 
    else (fun _ -> false) in *)
  let check_yi = Bstats.time "Z3 implies(1)" TheoremProverZ3.Prover.implies p in
  fun q -> 
    let q = incr nb_queries; Bstats.time "fixdiv" fixdiv q in
    Bstats.time "Z3 implies(2)" check_yi q 
     (* incr nb_cache_miss;
      if !Cl.use_qprover && Bstats.time "check_qp" check_qp q then true else 
        let rv = Bstats.time "YI implies(2)" check_yi q in
        (if rv then 
          let _ = incr nb_qp_miss in
          C.cprintf C.ol_solve "@[QP fails:@ %s @]" (imp_to_string p q)); rv *)

let finish () = 
  TheoremProverZ3.Prover.finish ()
	
let model p = 
	TheoremProverZ3.Prover.model p
	
let sat p = 
	TheoremProverZ3.Prover.sat p
	
let unsat p = 
	TheoremProverZ3.Prover.unsatisfiable p
		
let push p = TheoremProverZ3.Prover.push p

let pop () = TheoremProverZ3.Prover.pop ()	

(* {{{

module Cl = Clflags
module DefaultProver = TheoremProverYices.Prover
module BackupProver = TheoremProverYices.Prover (* shouldn't this be simplify?*)

exception Provers_disagree of bool * bool

let check_result f g arg =
  if not !Cl.check_queries then Bstats.time "calling PI" f arg else
    let fres = f arg in
    let gres = g arg in
      if fres != gres then raise (Provers_disagree (fres, gres))
      else fres

let do_both_provers f g arg =
  f arg; if !Cl.check_queries then g arg else ()

let push p = do_both_provers DefaultProver.push BackupProver.push (fixdiv p)

let pop () = do_both_provers DefaultProver.pop BackupProver.pop ()

let valid p = check_result DefaultProver.valid BackupProver.valid (fixdiv p)

let check_table p q =
  let ipq = P.implies (p, q) in
    if Hashtbl.mem qcache ipq then (incr hits;(true, Hashtbl.find qcache ipq))
                              else (false, false)

let check_imps p qs = 
      List.map (fun q -> implies (p,q)) qs

let check_implies default backup p q =
  
  let _ = incr num_queries in
  let use_cache = !Cl.cache_queries in
  let ipq = P.implies (p, q) in

  let cached = (Bstats.time "cache lookup" (Hashtbl.mem qcache) ipq) && use_cache in
  let _ = if cached then incr hits in

  let (p, q) = Bstats.time "fixing div" (fun () -> (fixdiv p, fixdiv q)) () in
  let res = if cached then Bstats.time "finding in cache " (Hashtbl.find qcache) ipq
                else check_result default backup (p, q) in
    if !Cl.dump_queries then
      Format.printf "@[%s%a@;<1 0>=>@;<1 0>%a@;<1 2>(%B)@]@.@."
        (if cached then "cached:" else "") P.pprint p P.pprint q res;
    (if cached then () else if use_cache then Hashtbl.replace qcache ipq res); res

let implies p q =
  if !Cl.always_use_backup_prover then
    Bstats.time "TP.ml prover query" (check_implies BackupProver.implies DefaultProver.implies p) q
  else
    Bstats.time "TP.ml prover query" (check_implies DefaultProver.implies BackupProver.implies p) q

let backup_implies p q = check_implies BackupProver.implies DefaultProver.implies p q

 }}} *)
