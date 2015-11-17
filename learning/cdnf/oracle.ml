(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************)

open Minisat

type result_t = bool array option

let make_conj solver vars outb =
  match vars with
    | [| |] -> add_clause solver [| pos_lit solver outb |]
    | [| var |] -> 
        (add_clause solver [| pos_lit solver outb; neg_lit solver var |];
         add_clause solver [| pos_lit solver var; neg_lit solver outb |])
    | _ ->
        let pos_outb = pos_lit solver outb in
        let neg_outb = neg_lit solver outb in
        let neg_vars = Array.map (neg_lit solver) vars in
          (add_clause solver (Array.append [| pos_outb |] neg_vars);
           Array.iter 
             (fun var -> add_clause solver [| neg_outb; pos_lit solver var |]) 
             vars)

let make_disj solver vars outb =
  match vars with
    | [| |] -> add_clause solver [| neg_lit solver outb |]
    | [| var |] -> 
        (add_clause solver [| pos_lit solver outb; neg_lit solver var |];
         add_clause solver [| pos_lit solver var; neg_lit solver outb |])
    | _ ->
        let pos_outb = pos_lit solver outb in
        let neg_outb = neg_lit solver outb in
        let pos_vars = Array.map (pos_lit solver) vars in
          (add_clause solver (Array.append [| neg_outb |] pos_vars);
           Array.iter 
             (fun var -> add_clause solver [| pos_outb; neg_lit solver var |])
             vars)

let make_neg solver var outb =
  (add_clause solver [| pos_lit solver var; pos_lit solver outb |];
   add_clause solver [| neg_lit solver var; neg_lit solver outb |])

let make_iff solver var outb =
    (add_clause solver [| pos_lit solver var; neg_lit solver outb |];
     add_clause solver [| pos_lit solver outb; neg_lit solver var |])

let make_lit solver var pos outb =
  if pos then make_iff solver var outb
  else make_neg solver var outb

let array_iter2 f ary0 ary1 =
  let _ = assert (Array.length ary0 = Array.length ary1) in
  let len = Array.length ary0 in
  let rec helper i =
    if i = len then ()
    else (f ary0.(i) ary1.(i); helper (succ i)) 
  in
    helper 0

let to_eq_sat solver vars f =
  let rec helper f outb =
    match f with
      | BoolFormula.Lit l ->
          let mvar = vars.(BoolFormula.of_var l) in
          make_lit solver mvar (BoolFormula.is_positive l) outb
      | BoolFormula.Not g ->
          let outb' = new_inferred_var solver in
            (helper g outb'; make_neg solver outb' outb)
      | BoolFormula.And fs -> 
          let outbs' = Array.map (fun _ -> new_inferred_var solver) fs in
            (make_conj solver outbs' outb; array_iter2 helper fs outbs')
      | BoolFormula.Or fs ->
          let outbs' = Array.map (fun _ -> new_inferred_var solver) fs in
            (make_disj solver outbs' outb; array_iter2 helper fs outbs')
  in
  let outb = new_inferred_var solver in
  let _ = helper f outb in
    outb

let create_dvars solver nvars =
  let helper i = Minisat.new_var solver in
    Array.init (succ nvars) helper 

let get_assignment solver vars =
  let helper i = Minisat.get_witness solver vars.(i) in
  let result = Array.init (Array.length vars) helper in
  let _ = result.(0) <- false in
    result

let assignment_to_lits solver vars assignment =
  let helper i b = 
    if b then pos_lit solver vars.(i) else neg_lit solver vars.(i) in
    Array.mapi helper assignment

let is_satisfiable_with_assumption nvars f assignment =
  let solver = Minisat.new_solver () in
  let vars = create_dvars solver nvars in
  let outf = to_eq_sat solver vars f in
  let _ = add_clause solver [| pos_lit solver outf |] in
  let lits = assignment_to_lits solver vars assignment in
    if solve_with_assumption solver lits then 
      Some (get_assignment solver vars)
    else 
      None

let is_equivalent nvars f g =
  let solver = Minisat.new_solver () in
  let vars = create_dvars solver nvars in
  let outf = to_eq_sat solver vars f in
  let outg = to_eq_sat solver vars g in
  let _ = add_clause solver [| pos_lit solver outf; pos_lit solver outg |] in
  let _ = add_clause solver [| neg_lit solver outf; neg_lit solver outg |] in
    if solve solver then
      Some (get_assignment solver vars)
    else
      None

let is_statisfiable nvars f = 
	let solver = Minisat.new_solver () in
	let vars = create_dvars solver nvars in
	let outf = to_eq_sat solver vars f in
	let _ = add_clause solver [| pos_lit solver outf |] in
	if solve solver then
		Some (get_assignment solver vars)
	else 
		None
