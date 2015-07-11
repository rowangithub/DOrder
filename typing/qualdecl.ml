open Parsetree
open Predicate


module C = Common
module T = Types
module Ct = Ctype
module AM = Map.Make(String)

let findam id m = if AM.mem id m then AM.find id m else Qualgen.TS.empty 

let fa env m (id, ty) =
  let s = findam id m in
  AM.add id (Qualgen.TS.add (Typetexp.transl_type_scheme env ty) s) m 

let lflun = List.fold_left Qualgen.IS.union Qualgen.IS.empty 

let lst s k = s::k

let conflat y = String.concat "." (Longident.flatten y)

let rel_star = [Ge; Le; Ne]
let op_star = [Plus; Minus; Times; Div; Mod]
let transl_ops ops = 
  match ops with 
      [] -> op_star 
    | _ -> List.map transl_op ops
let transl_rels rels = 
  match rels with 
      [] -> rel_star 
    | _ -> List.map transl_rel rels

let transl_patpred_single simple valu env p =
  let penv = ref [(Path.name valu, valu)] in
  let ap p pid = penv := (p, pid)::!penv; pid in
  let fep p = fst (Env.lookup_value p env) in
  let fp p' = 
    let p = conflat p' in
      try snd (List.find (fun (p', pid) -> p = p') !penv) with 
        Not_found -> try let ep = fep p' in ap p ep with
          Not_found -> let pid = Path.mk_ident p in 
            ap p pid in
  let rec transl_expr_rec pe =
    match pe.ppredpatexp_desc with
      | Ppredpatexp_int (n) ->
          let _ = if List.length n != 1 then assert false in
	        PInt (List.hd n)
      | Ppredpatexp_var (y) ->
          let y = match y with [sty] -> sty | _ -> failwith "Var ident set used in single qualifier or predicate" in
	        Var (if simple then Path.mk_ident (conflat y) else fp y)
      | Ppredpatexp_funapp (f, es) ->
	        FunApp (conflat f, List.map transl_expr_rec es)
      | Ppredpatexp_binop (e1, ops, e2) ->
	        Binop (transl_expr_rec e1, transl_op (List.hd ops), transl_expr_rec e2)
      | Ppredpatexp_field (f, e1) ->
          Field (f, transl_expr_rec e1)
      | Ppredpatexp_proj (n, e1) ->
          Proj (n, transl_expr_rec e1)
      | _ -> failwith "Wildcard used in single qualifier or predicate"
  in
  let rec transl_pred_rec pd =
    match pd.ppredpat_desc with
      | Ppredpat_true -> 
          True
      | Ppredpat_atom (e1, rels, e2) ->
	        Atom (transl_expr_rec e1, transl_rel (List.hd rels), transl_expr_rec e2)
      | Ppredpat_not (p) -> 
          Not (transl_pred_rec p)
      | Ppredpat_and (p1, p2) -> 
          And (transl_pred_rec p1, transl_pred_rec p2)
      | Ppredpat_or (p1, p2) -> 
          Or (transl_pred_rec p1, transl_pred_rec p2)
			| Ppredpat_forall (ps, pred) -> 
					(*let ps = List.map (fun (str, ty) ->  
						let p = match ty.ptyp_desc with
							| Ptyp_var ty -> Path.Pident (Ident.create_persistent ty)
							| ty -> (
								Format.fprintf Format.std_formatter "non-constructor types used in for-all formula@."; 
								assert false) in
						let id = Ident.create_persistent str in
						(Path.Pident id, p)) ps in*)
					let ps = List.map transl_expr_rec ps in
					let ps = List.map Predicate.exp_var ps in
					let pred = transl_pred_rec pred in
					Forall (ps, pred)
			| Ppredpat_reach (d, u) ->
				let d = transl_expr_rec d in
				let u = transl_expr_rec u in
				Reach (d, u)
			| Ppredpat_link (d, cstr, field, u, v) ->
				(let d = transl_expr_rec d in
				let u = transl_expr_rec u in
				let v = transl_expr_rec v in
				Link (d, conflat cstr, field, u, v))
  in transl_pred_rec p

(* unifying does something odd with state, we'll see how this works.. *)
let unifies env a b = try (Ct.unify env a b; true) with Ct.Unify(_) -> false
                                                      | Ct.Tags(_, _) -> false

let get_ids_by_type env qtys ptys qtymap =
  let qtys = Qualgen.TS.elements qtys in
  let ty_eqs qt pt = C.same_type pt qt || unifies env qt pt in
  let get_ids qt = 
    let mat_ty = List.filter (fun pt -> ty_eqs qt pt) ptys in 
    let tyset ty = Qualgen.findm qtymap ty in
    let ids = List.map tyset mat_ty in
      lflun ids in
  lflun (List.map get_ids qtys)
             
let transl_patpred env (v, nv) (qgtymap, tyset, idset, intset) tymap p =
  let all_consts = lazy (Qualgen.CS.elements intset) in
  let all_ids = lazy (Qualgen.IS.elements idset) in
  let all_tys = lazy (Qualgen.TS.elements tyset) in

  let rec transl_expr_rec pe =
    match pe.ppredpatexp_desc with
      | Ppredpatexp_int (n) ->
	        PPInt (n)
      | Ppredpatexp_any_int ->
          PPInt (Lazy.force all_consts)      
      | Ppredpatexp_var (y) -> 
          let flat_or_nv y =
            let y = conflat y in
            let y = if y = v then nv else y in
            Path.mk_ident y in
	        PVar (List.map flat_or_nv y)
      | Ppredpatexp_mvar (y) ->
          let inty = AM.mem y tymap in
          let mk_idents = List.map Path.mk_ident in
          let found_ids = lazy (get_ids_by_type env (AM.find y tymap) (Lazy.force all_tys) qgtymap) in 
            if inty then
              PVar (mk_idents (Qualgen.IS.elements (Lazy.force found_ids)))
            else 
              PVar (mk_idents (Lazy.force all_ids)) 
      | Ppredpatexp_funapp (f, es) ->
	        PFunApp (f, List.map transl_expr_rec es)
      | Ppredpatexp_binop (e1, ops, e2) ->
	        PBinop (transl_expr_rec e1, transl_ops ops, transl_expr_rec e2)
      | Ppredpatexp_field (f, e1) ->
          PField (f, transl_expr_rec e1)
      | Ppredpatexp_proj (n, e1) ->
          PProj (n, transl_expr_rec e1)
  in
  let rec transl_pred_rec pd =
    match pd.ppredpat_desc with
      | Ppredpat_true -> 
          PTrue
      | Ppredpat_atom (e1, rels, e2) ->
	        PAtom (transl_expr_rec e1, transl_rels rels, transl_expr_rec e2)
      | Ppredpat_not (p) -> 
          PNot (transl_pred_rec p)
      | Ppredpat_and (p1, p2) -> 
          PAnd (transl_pred_rec p1, transl_pred_rec p2)
      | Ppredpat_or (p1, p2) -> 
          POr (transl_pred_rec p1, transl_pred_rec p2)
			| Ppredpat_forall (ps, pred) -> (* ps is (string * core_type) list *)
					(*let ps = List.map (fun (str, ty) -> 
						let p = match ty.ptyp_desc with
							| Ptyp_var ty -> Path.Pident (Ident.create_persistent ty)
							| ty -> (
								Format.fprintf Format.std_formatter "non-constructor types used in for-all formula@."; 
								assert false) in
						let id = Ident.create_persistent str in
						(Path.Pident id, p)) ps in*)
					let ps = List.map transl_expr_rec ps in
					let ps = List.map (fun p -> 
						match p with 
							| PVar ps -> (assert (List.length ps = 1); List.hd ps) | _ -> assert false) ps in
					let pred = transl_pred_rec pred in
					PForall (ps, pred)
			| Ppredpat_reach (d, u) ->
				let d = transl_expr_rec d in
				let u = transl_expr_rec u in
				PReach (d, u)
			| Ppredpat_link (d, cstr, field, u, v) ->
				(let d = transl_expr_rec d in
				let u = transl_expr_rec u in
				let v = transl_expr_rec v in
				PLink (d, conflat cstr, field, u, v))
  in transl_pred_rec p

let rec lflap es =
  match es with
    | s :: [] ->
        List.map (fun c -> [c]) s
    | s :: es ->
        C.flap (fun c -> List.map (fun d -> c :: d) (lflap es)) s
    | [] ->
        []

let tflap3 (e1, e2, e3) f =
  C.flap (fun c -> C.flap (fun d -> List.map (fun e -> f c d e) e3) e2) e1

let tflap2 (e1, e2) f =
  C.flap (fun c -> List.map (fun d -> f c d) e2) e1

let gen_preds p =
  let rec gen_expr_rec pe =
    match pe with
      | PPInt (ns) ->
          List.map (fun c -> PInt (c)) ns  
      | PVar (ps) ->
          List.map (fun c -> Var (c)) ps
      | PFunApp (f, es) ->
          let f' = conflat f in
          let ess = List.map gen_expr_rec es in
            List.map (fun e -> FunApp (f', e)) (lflap ess) 
      | PBinop (e1, ops, e2) ->
          let e1s = gen_expr_rec e1 in
          let e2s = gen_expr_rec e2 in
            tflap3 (e1s, ops, e2s) (fun c d e -> Binop (c, d, e))
      | PField (f, e1) ->
          let e1s = gen_expr_rec e1 in
            List.map (fun e -> Field(f, e)) e1s
      | PProj (n, e1) ->
          let e1s = gen_expr_rec e1 in
            List.map (fun e -> Proj(n, e)) e1s
  in    
  let rec gen_pred_rec pd =
    match pd with
      | PTrue ->
          [True] 
      | PNot (p) ->  
          List.map (fun c -> Not (c)) (gen_pred_rec p) 
      | POr (p1, p2) ->
          let p1s = gen_pred_rec p1 in
          let p2s = gen_pred_rec p2 in
            tflap2 (p1s, p2s) (fun c d -> Or (c, d))
      | PAnd (p1, p2) ->
          let p1s = gen_pred_rec p1 in
          let p2s = gen_pred_rec p2 in
            tflap2 (p1s, p2s) (fun c d -> And (c, d))
      | PAtom (e1, rels, e2) ->      
          let e1s = gen_expr_rec e1 in
          let e2s = gen_expr_rec e2 in
            tflap3 (e1s, rels, e2s) (fun c d e -> Atom (c, d, e))
      | PIff (e1, p1) ->
          let e1s = gen_expr_rec e1 in
          let p1s = gen_pred_rec p1 in
            tflap2 (e1s, p1s) (fun c d -> Iff (c, d))
			| PForall (ps, pred) ->
					List.map (fun pred -> Forall (ps, pred)) (gen_pred_rec pred)
			| PReach (d, u) ->
					let ds = gen_expr_rec d in
					let us = gen_expr_rec u in
					tflap2 (ds, us) (fun d u -> Reach (d, u))
			| PLink (d, cstr, field, u, v) ->
					let ds = gen_expr_rec d in
					let us = gen_expr_rec u in
					let vs = gen_expr_rec v in
					tflap3 (ds, us, vs) (fun d u v -> Link (d, cstr, field, u, v))
  in gen_pred_rec p

let ck_consistent patpred pred =
  let m = ref [] in
  let addm a = m := a::!m in
  let gtm (a, b) = 
    try List.find (fun (c, _) -> a = c) !m 
      with Not_found -> addm (a, b); (a,b) in
  let ckm (a, b) = (fun (_, d) -> b = d) (gtm (a, b)) in
  let rec ck_expr_rec pred pat =
    match (pred.ppredpatexp_desc, pat) with
      | (Ppredpatexp_var (_), Var(_))
      | (Ppredpatexp_any_int, PInt (_)) 
      | (Ppredpatexp_int (_), PInt (_)) ->
	        true
      | (Ppredpatexp_funapp (_, es), FunApp (_, el)) ->
          List.for_all2 ck_expr_rec es el
      | (Ppredpatexp_binop (e1, _, e2), Binop (e1', _, e2')) ->
          ck_expr_rec e1 e1' && ck_expr_rec e2 e2'  
      | (Ppredpatexp_field (_, e1), Field(_, e1')) ->
          ck_expr_rec e1 e1'
      | (Ppredpatexp_mvar (x), Var(y)) ->
          ckm (x, Path.name y)
      | (Ppredpatexp_proj (_, e), Proj (_, e')) ->
          ck_expr_rec e e'
      | _ -> assert false in
  let rec ck_pred_rec pred pat =
    match (pred.ppredpat_desc, pat) with
      | (Ppredpat_true, True) -> 
          true
      | (Ppredpat_atom (e1, _, e2), Atom (ee1, _, ee2)) ->
          ck_expr_rec e1 ee1 && ck_expr_rec e2 ee2
      | (Ppredpat_not (p), Not (pp)) -> 
          ck_pred_rec p pp
      | (Ppredpat_or (p1, p2), Or (pp1, pp2))
      | (Ppredpat_and (p1, p2), And (pp1, pp2)) -> 
          ck_pred_rec p1 pp1 && ck_pred_rec p2 pp2 
			| (Ppredpat_forall (ps, pred), Forall (ps', pred')) ->
				  ck_pred_rec pred pred'
			| (Ppredpat_reach (d, u), Reach (d', u')) ->
					(ck_expr_rec d d') && (ck_expr_rec u u')
			| (Ppredpat_link (d, cstr, f, u, v), Link (d', cstr', f', u', v')) ->
					(String.compare (conflat cstr) cstr' = 0) && (f = f') &&
					(ck_expr_rec d d') && (ck_expr_rec u u') && (ck_expr_rec v v')
      | _ -> assert false in
    ck_pred_rec patpred pred
     

(* Translate a qualifier declaration *)
let transl_pattern env prgids {Parsetree.pqual_pat_desc = (v, anno, pred)} nv =
  let preds = (gen_preds (transl_patpred env (v, nv) prgids (List.fold_left (fa env) AM.empty anno) pred)) in
    List.filter (fun p -> ck_consistent pred p) preds


let transl_pattern_valu env prgids name ({Parsetree.pqual_pat_desc = (valu, anno, pred)} as p) =
	let normal_valu = "_V" in
  let num = ref 0 in
  let fresh name = incr num; name ^ (string_of_int !num) in
  let preds = transl_pattern env prgids p normal_valu in
    List.map (fun p -> (fresh name, normal_valu, p)) preds
