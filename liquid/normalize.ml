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

open Parsetree
open Asttypes
open Format


let wrap_printable exp = (Ptop_def([{pstr_desc = (Pstr_eval exp); pstr_loc = Location.none}])) 
let dummy = Location.none

(* we can't avoid a global counter here: generate unique names *)
let ncnt = ref 0 
let fresh_name_s () = incr ncnt; "__atmp" ^ (string_of_int !ncnt) 
let fresh_name () = Longident.parse (fresh_name_s ())
 
    
(* ming: I think it's actually better to pass everything around as
 * longidents for true generality, but applies of functors should never
 * happen in our code so this assumption is OK *)
let li_flatten li = String.concat "." (Longident.flatten li) 

(* constant divisions and multiplications are wreaking havoc on us
 * because non-const ops turn into uninterpreted functions and make
 * life difficult *)
let is_op exp nm = 
  match exp.pexp_desc with
    | Pexp_ident(id) ->
        li_flatten id = nm
    | _ -> false

let is_mult exp = 
  is_op exp "*"
let is_div exp =
  is_op exp "/"
               
let is_const exp =
  match exp.pexp_desc with
    | Pexp_constant(Const_int _) ->
        true
    | _ -> false

let is_function exp =
  match exp.pexp_desc with
    | Pexp_function(_, _, _) ->
        true
    | _ -> false

let is_const_div exp = 
  match exp.pexp_desc with 
    Pexp_apply(e1, es) ->
      let es = List.map (fun (_, e) -> e) es in
      let div = is_div e1 in
        if div then is_const (List.nth es 1) else false 
    | _ -> false
      
let is_const_mult exp =
  match exp.pexp_desc with
    Pexp_apply(e1, es) ->
      let es = List.map (fun (_, e) -> e) es in
      let mult = is_mult e1 in
        if mult then is_const (List.nth es 1) || is_const (List.hd es)
        else false
    | _ -> false


(* ming: we dummy out all the pattern locations because we don't use them.
 * this is technically destructive though, if we do implement pattern matching
 * it will have to be fixed. *)
let mk_dum_varpat x = {ppat_desc = Ppat_var x; ppat_loc = dummy}

let mk_let_lit r pes e2 = Pexp_let(r, pes, e2)
let mk_let r x e1 e2 = mk_let_lit r [({ppat_desc = Ppat_var x; ppat_loc = dummy}, e1)] e2
let mk_let_lbl r x e1 e2 = mk_let r (li_flatten x) e1 e2
let mk_apply e1 es = Pexp_apply(e1, (List.map (fun e -> ("", e)) es))
let mk_ident id = Pexp_ident(id)
let mk_function lbl elbl arg_pat sube = Pexp_function(lbl, elbl, [(arg_pat, sube)])
let mk_array es = Pexp_array(es) 
let mk_tuple es = Pexp_tuple(es)
let mk_sequence e1 e2 = Pexp_sequence(e1, e2)
let mk_ifthenelse e1 e2 e3 = Pexp_ifthenelse(e1, e2, Some e3)
let mk_field e s = Pexp_field(e, s)
let mk_record es = Pexp_record(es, None)
let mk_assert e = Pexp_assert(e)
let mk_match e pel = Pexp_match(e, pel)
let mk_construct cstrdesc e b = Pexp_construct(cstrdesc, Some e, b)

let mk_dummy desc loc = {pexp_desc = desc; pexp_loc = loc}

let mk_dum_ident id loc = mk_dummy (mk_ident id) loc
let mk_ident_loc id loc = {pexp_desc = mk_ident id; pexp_loc = loc}

let resolve_in_exp_when f ls =
  let (lbl, lex, loc) = List.hd ls in
  match lex with
    | Some lex when f lex -> (lex, List.tl ls)
    | _ -> (mk_ident_loc lbl loc, ls) 

let resolve_in_exp = resolve_in_exp_when (fun e -> true) 
 

let normalize exp =
 let rec norm_out exp =
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let wrap r b (lbl, a, _) = 
      match a with
          Some a -> mk_let_lbl r lbl a (mk_dummy b exp.pexp_loc)
          | None -> b
    in
    let proc_list es skel =
     let lss = List.map norm_in es in
     let (lbls, lss) = List.fold_right (fun ls (ess, lss) ->
                                        let (e, ls) = resolve_in_exp_when is_const ls in
                                          (e::ess, ls::lss)) lss ([], []) in
     let init = skel lbls in
      rw_expr (List.fold_left (wrap Nonrecursive) init (List.concat (List.rev lss)))
    in

    match exp.pexp_desc with
     | Pexp_constant(_) 
     | Pexp_construct(_, None, _) ->
         (* no normalizing of simple constants and constructs for now *)
         exp
     | Pexp_construct(cstrdesc, Some e, b) ->
         let ls = norm_in e in
         let (inex, ls) = resolve_in_exp ls in
         let init = mk_construct cstrdesc inex b in
          rw_expr (List.fold_left (wrap Nonrecursive) init ls)
     | Pexp_constraint(_, _, _)
     | Pexp_ident(_) ->
        exp
     | Pexp_function(lbl, elbl, [(arg, e)]) ->
        rw_expr (mk_function lbl elbl arg (norm_out e))
     | Pexp_let(Recursive, pes, e2) ->
        (* we can assume more or less that all recursive ands are 
         * binds of mutually recursive functions, so we won't even try
         * to norm_in them *)
        let pes = List.map (fun (p, e) -> (p, norm_out e)) pes in
          rw_expr (mk_let_lit Recursive pes (norm_out e2))
     | Pexp_let(Nonrecursive, pes, e2) ->
         let mk_lbl (p, e) = 
           let ls = norm_in e in
           let (lbl, _, lo) = List.hd ls in
           let lbl = mk_ident_loc lbl lo in
             (lbl, ls) in
         let lbss = List.map mk_lbl pes in
         let pes = List.map2 (fun (p, e) (lbl, ls) -> (p, lbl)) pes lbss in
         let lss = List.map (fun (lbl, ls) -> ls) lbss in
         let init = mk_let_lit Nonrecursive pes (norm_out e2) in 
           rw_expr (List.fold_left (wrap Nonrecursive) init (List.concat (List.rev lss)))
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, _, lo) = List.hd f in 
        let es = List.map (fun (_, e) -> e) es in
        let lss = List.map norm_in es in
        let ts = List.map (fun ls -> let (lbl, _, lo) = List.hd ls in mk_ident_loc lbl lo) lss in
          (* ming: hack for constant div -- we'll be able to clean this up if we
          * stick with never normalizing constants *)
        let ts = if is_const_div exp || is_const_mult exp then
                    let e_n1 = List.hd ts in
                    let e_n2 = List.nth ts 1 in
                    let e_o1 = List.hd es in
                    let e_o2 = List.nth es 1 in
               [if is_const e_o1 then e_o1 else e_n1;
                if is_const e_o2 then e_o2 else e_n2]
               else ts in
        let init = mk_apply (mk_ident_loc flbl lo) ts in
        let ls = List.concat (List.rev (f::lss)) in
         rw_expr (List.fold_left (wrap Nonrecursive) init ls)  
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        let (e_b, b) = resolve_in_exp (norm_in e1) in 
        let init = mk_ifthenelse e_b (norm_out e2) (norm_out e3) in
         rw_expr (List.fold_left (wrap Nonrecursive) init b)
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array
          (* lose location for the whole sequence, but that's generally ok since
           * we don't lose location of second expression *)
     | Pexp_sequence(e1, e2) ->
        rw_expr (mk_sequence (norm_out e1) (norm_out e2))
     | Pexp_assertfalse ->
        exp
     | Pexp_assert(e) ->
        let c = norm_in e in 
        let (lbl, e', lo) = List.hd c in
        let inner = 
          match e' with 
          | Some e -> e
          | None -> mk_ident_loc lbl lo
        in
        let init = mk_assert inner in
          rw_expr (List.fold_left (wrap Nonrecursive) init (List.tl c))
     | Pexp_field(e, s) ->
        let ls = norm_in e in
        let (lbl, _, lo) = List.hd ls in
        let init = mk_field (mk_ident_loc lbl lo) s in
          rw_expr (List.fold_left (wrap Nonrecursive) init ls)
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in 
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let init = mk_record es' in
        let ee = List.concat (List.rev ee) in
          rw_expr (List.fold_left (wrap Nonrecursive) init ee)
     | Pexp_match(e, pel) ->
        let npel = List.map (fun (p, e) -> (p, norm_out e)) pel in
        let ls = norm_in e in
        let (lbl, _, lo) = List.hd ls in
        let init = mk_match (mk_ident_loc lbl lo) npel in
          rw_expr (List.fold_left (wrap Nonrecursive) init ls)
     | e -> printf "@[Bad expr to norm_out:@\n%a@]" Qdebug.pprint_expression exp; flush stdout; assert false

  and norm_in exp = 
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let proc_list es skel = 
      let this = fresh_name () in
      let lss = List.map norm_in es in
      let (lbls, lss) = List.fold_right (fun ls (ess, lss) ->
                                          let (e, ls) = resolve_in_exp_when is_const ls in
                                            (e::ess, ls::lss)) lss ([], []) in
      let e_this = Some (rw_expr (skel lbls)) in
        (this, e_this, dummy)::(List.concat (List.rev lss))
    in
    let loc = exp.pexp_loc in

    match exp.pexp_desc with
     | Pexp_assert(_)  
     | Pexp_assertfalse ->
         [(fresh_name (), Some (norm_out exp), dummy)]
     | Pexp_constraint(_, _, _)
     | Pexp_constant(_) 
     | Pexp_construct(_, None, _) ->
         [(fresh_name (), Some exp, dummy)]
     | Pexp_construct(cstrdesc, Some e, b) ->
         let ls = norm_in e in
         let (inex, ls) = resolve_in_exp ls in
         let this = fresh_name () in
         let e_this = Some (rw_expr (mk_construct cstrdesc inex b)) in
         (this, e_this, loc)::ls
     | Pexp_ident(id) ->
         (* ming: to preserve annotations over idents we actually want something
          * like (id, exp), but that will add another layer to all scopes and
          * generally make life more difficult while debugging. will turn this
          * off eventually *)
         [(id, None, loc)]
     | Pexp_function(_, _, _)     
     | Pexp_let(_, _, _) -> 
        [(fresh_name (), Some (norm_out exp), dummy)]
          (* ming: pull sequences out to the closest scope *)
     | Pexp_sequence(e1,e2) ->
        let ls1 = norm_in e1 in
        let ls2 = norm_in e2 in
          List.append ls2 ls1
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array 
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, e_f, lo_f) = List.hd f in
        let es = List.map (fun (_, e) -> e) es in
        let fn = match e_f with Some e -> mk_dum_ident flbl loc
                                | None -> mk_ident_loc flbl lo_f in
        let ls = proc_list es (mk_apply fn) in
        let (this, e_this, lo_this) = List.hd ls in
          (* ming: hack for constant div *)
        let e_this =  
          if is_const_div exp || is_const_mult exp then 
            begin
            match e_this with
                Some e_this ->
                    begin match e_this.pexp_desc with
                    | Pexp_apply(e1, es') ->
                      let e_n1 = List.hd es' in
                      let e_n2 = List.nth es' 1 in
                      let e_o1 = List.hd es in
                      let e_o2 = List.nth es 1 in
                      let fst = if is_const e_o1 then ("", e_o1) else e_n1 in
                      let snd = if is_const e_o2 then ("", e_o2) else e_n2 in
                      let es' = [fst; snd] in
                      Some {pexp_desc = Pexp_apply(e1, es'); 
                            pexp_loc = e_this.pexp_loc}
                    | _ -> assert false
                    end
                | None -> assert false
            end
          else e_this in 
          (this, e_this, lo_this)::(List.append (List.tl ls) f)
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        let (e_b, b) = resolve_in_exp (norm_in e1) in
        let (this, e_this, lo_this) = 
          (fresh_name (), mk_ifthenelse e_b (norm_out e2) (norm_out e3), dummy) in
         (this, Some (rw_expr e_this), lo_this)::b
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let e_this = rw_expr (mk_record es') in
          (fresh_name (), Some e_this, loc)::(List.concat (List.rev ee)) 
     | Pexp_field(e, s) ->
        let ls = norm_in e in 
        let (lbl, _, lo) = List.hd ls in
          (fresh_name (), Some (rw_expr (mk_field (mk_ident_loc lbl lo) s)), loc)::ls
     | Pexp_match(e, pel) ->
         let npel = List.map (fun (p, e) -> (p, norm_out e)) pel in
         let ls = norm_in e in
         let (lbl, _, lo) = List.hd ls in
          (fresh_name (), Some (rw_expr (mk_match (mk_ident_loc lbl lo) npel)), loc)::ls
     | e -> printf "@[Bad expr to norm_in:@\n%a@]" Printast.top_phrase (wrap_printable exp); assert false
  in
  norm_out exp



let rec normalize_structure sstr =
 let _ = if Common.ck_olev Common.ol_normalized then Format.set_margin 170 in
  match sstr with
    [] -> []
    | {pstr_desc = (Pstr_eval exp); pstr_loc = loc} :: srem ->
        let normal_exp = normalize exp in
        let _ = Common.cprintf Common.ol_normalized "@[%a@\n@]@." Qdebug.pprint_expression normal_exp in
        ({pstr_desc = (Pstr_eval(normal_exp)) ; pstr_loc = loc}) :: (normalize_structure srem)
    | {pstr_desc = (Pstr_value(recursive, pl)); pstr_loc = loc} :: srem -> 
        let pl = List.map (fun (p, e) -> (p, normalize e)) pl in
        let value = {pstr_desc = (Pstr_value(recursive, pl)); pstr_loc = loc} in
        let _ = Common.cprintf Common.ol_normalized "@[%a@\n@]@." Qdebug.pprint_structure value in
          value :: (normalize_structure srem) 
    | p :: srem -> 
        p :: (normalize_structure srem) 
