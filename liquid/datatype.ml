(* Dealing with the data-type measures *)
open Asttypes
open Typedtree
open Btype
open Types

open Backwalker
open Constraint

let forall_uvar = Path.mk_ident "u"
let forall_uexpr = Predicate.Var forall_uvar
let forall_vvar = Path.mk_ident "v"
let forall_vexpr = Predicate.Var forall_vvar
let foralls = [forall_uvar; forall_vvar]

let return = Predicate.Atom (Predicate.Var (Frame.returnpath), Predicate.Eq, Backwalker.qual_test_expr)
let dummypath = Path.mk_ident ""

let rec get_funbody exp = match exp.exp_desc with
	| Texp_function ([(pat, e')], _) -> get_funbody e'
	| _ -> exp
			
let symbexe_match se_env fpath e = match e.exp_desc with
	| Texp_match (e, pexps, partial) -> 
		List.map (fun (pat, e) -> 
			let (name, args) = match pat.pat_desc with
				| Tpat_construct (constructor_desc, patterns) ->
					(constructor_desc.cstr_name, List.map (fun pattern -> 
						match pattern.pat_desc with Tpat_var id -> Path.Pident id | _ -> assert false
					) patterns)
				| _ -> assert false in
			let (_,pre,preturn, _) = 
				(*let pred = Predicate.Atom (Backwalker.qual_test_expr, Predicate.Eq, Predicate.PInt 0) in*)
				let oldpath = !(Backwalker.curr_function_path) in
				let _ = Backwalker.curr_function_path := fpath in
				let res = Backwalker.symb_exe (false, [dummypath]) se_env e ((Predicate.True), (*pred*)return, return, Predicate.True) in
				let _ = Backwalker.curr_function_path := oldpath in res in
			(name, args, (*preturn*)pre)
		) pexps
	| _ -> assert false		

(** Symbolically summarize the measure function *)
let symbexe_measure se_env env =
	let measures = se_env.measures in
	Hashtbl.iter (fun typath ms -> List.iter (fun (m, _) -> 
		(*let _ = Format.fprintf Format.std_formatter "working for measure %s@." (Path.name m) in*)
		let (_, body) = Hashtbl.find se_env.funbindings m in
		let body = get_funbody body in
		let measure_defs = symbexe_match se_env m body in
		List.iter (fun measure_def ->
			match measure_def with (name, args, preturn) ->
				((*Format.fprintf Format.std_formatter "constructor name=%s@." name;
				List.iter (fun arg -> Format.fprintf Format.std_formatter "arg = %s " (Path.name arg)) args;
				Format.fprintf Format.std_formatter "body = %a@." Predicate.pprint preturn;*)
				if (Hashtbl.mem se_env.measure_defs (typath, name)) then
					Hashtbl.replace se_env.measure_defs (typath, name) 
						((Path.name m, args, preturn)::(Hashtbl.find se_env.measure_defs (typath, name)))
				else Hashtbl.replace se_env.measure_defs (typath, name) [(Path.name m, args, preturn)]) 
		) measure_defs
	) ms) measures
	
(** Extract the type of values in a container *)
let extract_value_type_from_container_ty ty = 
	let rec loop p' tys = 
		let _ = assert (List.length tys = 1) in
		let ty = List.hd tys in
		match ty.desc with
			| Tconstr (p,tys,_) 
				when (tys <> []) -> loop p tys
			| _ -> p' in
	match ty with
		| Tconstr (p,tys,_) 
			when (tys <> []) -> (
			let _ = assert (List.length tys = 1) in
			let ty = List.hd tys in
			match ty.desc with
				| Tconstr (p',tys,_)
					when (tys <> []) -> 
					(false, loop p' tys)
				| _ ->  (true, p)
			)
		| Tconstr (p,_,_) -> (
			(false, p))
		| _ -> assert false


(** Extract the type of content *)
(** if a list does not contain another container, then do the list-unfonding
otherwise, a list does contain another containers, use the sub-container's unfolding *)
let extract_value_type_from_container_fr fr = 
	let rec loop p' fs = 
		let _ = assert (List.length fs = 1) in
		let f = List.hd fs in
		match f with
			| Frame.Fconstr (p,fs,_,_,_) 
				when (fs <> []) -> loop p fs
			| _ -> p' in
	match fr with
		| Frame.Fconstr (p,fs,_,_,_) 
			when (fs <> []) -> (
			let _ = assert (List.length fs = 1) in
			let f = List.hd fs in
			match f with
				| Frame.Fconstr (p',fs,_,_,_)
					when (fs <> []) -> 
					(false, loop p' fs)
				| _ ->  (true, p)
			)
		| Frame.Fconstr (p,_,_,_,_) -> (false, p)
		| _ -> assert false

(** Get alllinks for a data structure *)
let get_all_links udt_table path = 
	let declaration = Hashtbl.find udt_table path in
	let allparams = match declaration.type_kind with 
		| Type_variant decs -> decs
		| kind -> assert false in	
	List.fold_left (fun res (cstrname, params) -> 
		fst (List.fold_left (fun (res, index) param -> match param.desc with
			| Tconstr (p, _, _) -> 
				let (b, p') = extract_value_type_from_container_ty (param.desc) in
				if (b || Path.same path p') then (res @ [(cstrname, index)], index+1)
				else (res, index+1)
			| _ -> (res, index+1)) (res, 0) params)
		) [] allparams
			

let update_shape_predicates udt_table path cstrname args qual_test_expr = 
	let cstrname' = String.lowercase_ascii cstrname in
	let declaration = Hashtbl.find udt_table path in
	let (allparams, params) = match declaration.type_kind with 
		| Type_variant decs -> (decs, List.assoc cstrname decs)
		| kind -> assert false in	
	(* Type of allparams : (string * type_expr list) list *)
	(* Type of params : type_expr list *)
	let alllinks = List.fold_left (fun res (cstrname, params) -> 
		fst (List.fold_left (fun (res, index) param -> match param.desc with
			| Tconstr (p, _, _) -> 
				let (b, p') = extract_value_type_from_container_ty (param.desc) in
				if (b || Path.same path p') then (res @ [(cstrname, index)], index+1)
				else (res, index+1)
			| _ -> (res, index+1)) (res, 0) params)
		) [] allparams in
	let (links, values, _) = List.fold_left (fun (links, values, index) param -> 
		match param.desc with
			(*| Tconstr (p, _, _) when (Path.same p path) -> (links @ [index], values, index+1)
			| Tconstr (p, _, _) when (Path.same p Predef.path_int) -> (links, values, index+1) *)
			| Tconstr (p, _, _) -> 
				let (b, p') = extract_value_type_from_container_ty (param.desc) in
				if (b || Path.same path p') then (links @ [index], values, index+1)
				else (links, values, index+1)
			| _ -> (links, values @ [index], index+1)
		) ([], [], 0) params in
	(* for egdes \in alllinks/links *)
	let extra_tbl = Hashtbl.create 3 in
	let extra_links = List.filter (fun (n, i) -> 
		let res = n <> cstrname in
		let _ = if (res) then (
			if (Hashtbl.mem extra_tbl n) then
				Hashtbl.replace extra_tbl n ((Hashtbl.find extra_tbl n)@[i])
			else Hashtbl.replace extra_tbl n [i]
			) in
		res) alllinks in
	let extra_links = extra_links @ (Hashtbl.fold (fun n links res -> 
		res @ (fst (List.fold_left (fun (res, reslinks) link -> 
			res @ (List.map (fun reslink -> (* establish link --> reslink *)
				(n, int_of_string ((string_of_int link) ^ (string_of_int reslink)) )
				) reslinks), reslinks@[link]
			) ([], []) links))
		) extra_tbl []) in
	let extra_links = (List.map (fun (n, i) -> 
		let n' = String.lowercase_ascii n in
		Predicate.logic_equals (Predicate.Link (qual_test_expr,n',i,forall_uexpr,forall_vexpr))
			(Predicate.big_or (List.map (fun linkid -> Predicate.Link (List.nth args linkid, n',i,forall_uexpr,forall_vexpr)) links)) 
		) extra_links) in 	
	let core_links = match (links, values) with
		| ([], []) -> (* link and reach implies empty *) (
			let link_refinement1 = 
				Predicate.logic_equals (Predicate.Reach (qual_test_expr,forall_uexpr))
				(Predicate.Not Predicate.True) in
			let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
			link_refinement1
			) 
		| ([], hd::_) -> (* link implies empty but reach implies an element *) (
			let hd = (List.nth args hd) in
			let link_refinement1 = 
				Predicate.logic_equals (Predicate.Reach (qual_test_expr,forall_uexpr))
				(Predicate.Atom (hd,Predicate.Eq,forall_uexpr)) in
			let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
			link_refinement1
			)
		| (_::_, []) -> (* link and reach both imply somthing but the construtor do not have value *) (
			let link_refinement1 = 
				Predicate.logic_equals (Predicate.Reach (qual_test_expr,forall_uexpr))
				(Predicate.big_or (List.map (fun linkid -> Predicate.Reach (List.nth args linkid, forall_uexpr)) links)) in
			let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
			(* But we allow corss link ...  *)
			let link_refinement3 = 
					fst (List.fold_left (fun (res, reslinks) link -> 
						let ps = List.map (fun reslink -> (* there is a link relation from link to reslink *)
							let linkid = int_of_string ((string_of_int link) ^ (string_of_int reslink)) in
							let l = List.nth args link in
							let r = List.nth args reslink in
							Predicate.logic_equals (Predicate.Link (qual_test_expr,cstrname',linkid,forall_uexpr,forall_vexpr))
							(Predicate.Or (
								Predicate.And (
									Predicate.Reach (l, forall_uexpr),
									Predicate.Reach (r, forall_vexpr)),
								Predicate.big_or (List.map (fun linkid' -> Predicate.Link (List.nth args linkid', cstrname',linkid,forall_uexpr,forall_vexpr)) links)
								)
							)
						) reslinks in
						(res @ ps, reslinks @ [link])
						) ([], []) links) in
			let link_refinement3 = List.map (fun link -> Predicate.Forall (foralls, link)) link_refinement3 in
			Predicate.big_and (link_refinement1::link_refinement3)
			(*let link_refinement2 = Predicate.big_and (
				List.map (fun linkid -> 
					Predicate.logic_equals (Predicate.Link (qual_test_expr,cstrname,linkid,forall_uexpr,forall_vexpr))
					(Predicate.big_or (List.map (fun linkid -> Predicate.Link (List.nth args linkid, cstrname,linkid,forall_uexpr,forall_vexpr)) links)) 
				) links) in	
			let link_refinement2 = Predicate.Forall (foralls, link_refinement2) in 
			Predicate.And (link_refinement1, link_refinement2) *)
			)
		| (_, _) -> (* link and reach are in the most common sense *) (
			let hd = (List.nth args (List.hd values)) in
			let link_refinement1 = 
				Predicate.logic_equals (Predicate.Reach (qual_test_expr,forall_uexpr))
				(Predicate.Or (
					Predicate.Atom (hd,Predicate.Eq,forall_uexpr), 
					Predicate.big_or (List.map (fun linkid -> Predicate.Reach (List.nth args linkid, forall_uexpr)) links)
					)
				) in
			let link_refinement1 = Predicate.Forall ([forall_uvar], link_refinement1)	in
			let link_refinement2 = 
					List.map (fun linkid -> 
						Predicate.logic_equals (Predicate.Link (qual_test_expr,cstrname',linkid,forall_uexpr,forall_vexpr))
						(Predicate.Or ( 
							Predicate.And (
								Predicate.Atom (hd,Predicate.Eq,forall_uexpr), 
								Predicate.Reach (List.nth args linkid, forall_vexpr)),
							Predicate.big_or (List.map (fun linkid' -> Predicate.Link (List.nth args linkid', cstrname',linkid,forall_uexpr,forall_vexpr)) links)
							)
						) 
					) links in	
			let link_refinement3 = 
					fst (List.fold_left (fun (res, reslinks) link -> 
						let ps = List.map (fun reslink -> (* there is a link relation from link to reslink *)
							let linkid = int_of_string ((string_of_int link) ^ (string_of_int reslink)) in
							let l = List.nth args link in
							let r = List.nth args reslink in
							Predicate.logic_equals (Predicate.Link (qual_test_expr,cstrname',linkid,forall_uexpr,forall_vexpr))
							(Predicate.Or (
								Predicate.And (
									Predicate.Reach (l, forall_uexpr),
									Predicate.Reach (r, forall_vexpr)),
								Predicate.big_or (List.map (fun linkid' -> Predicate.Link (List.nth args linkid', cstrname',linkid,forall_uexpr,forall_vexpr)) links)
								)
							)
						) reslinks in
						(res @ ps, reslinks @ [link])
						) ([], []) links) in
			let link_refinement2 = List.map (fun link -> Predicate.Forall (foralls, link)) link_refinement2
				(*Predicate.Forall (foralls, link_refinement2)*) in
			let link_refinement3 = List.map (fun link -> Predicate.Forall (foralls, link)) link_refinement3 in
			Predicate.big_and (link_refinement1::(link_refinement2 @ link_refinement3))
			) in
	if (extra_links = []) then 
		core_links
	else 
		Predicate.And (core_links, Predicate.Forall (foralls, Predicate.big_and extra_links))
	
(** Using the measures to update the orginial coarse constraints *)
let update_measure_frame se_env udt_table p f = match (f, p) with
	| (Frame.Fconstr (path, ts, vs, ([], Frame.Qconst qs), eff), Some p) 
		when (Hashtbl.mem se_env.measures path && String.compare "dtyencoding" (Path.name p) = 0) -> (
		assert (List.length qs = 1);
		let q = List.hd qs in
		(*let _ = (Format.fprintf Format.std_formatter "dtyencoding = %a@." Qualifier.pprint q) in*)
		match q with
			| (a,b,Predicate.Atom ((*Predicate.Var*) matche, Predicate.Eq, Predicate.FunApp (cstrname, args))) -> 	
				if (Hashtbl.mem se_env.measure_defs (path, cstrname)) then
					let measures = Hashtbl.find se_env.measure_defs (path, cstrname) in
					let cstrdefs = List.map (fun (measure, cstrargs, cstrdef) ->
						let substs = List.map2 (fun cstrarg arg -> (cstrarg, arg)) cstrargs args in
						let substs = (Frame.returnpath, Predicate.FunApp (measure, [(*Predicate.Var*) matche]))::substs in
						Predicate.apply_substs substs cstrdef
					) measures in
					(*let _ = (Format.fprintf Format.std_formatter "then dtyencoding = %a@." Predicate.pprint (Predicate.big_and cstrdefs)) in*)
					if !(Clflags.reachability) then
						let shape = update_shape_predicates udt_table path cstrname args matche in
						Frame.set_refinement f 
							([], Frame.Qconst [(a, b, Predicate.big_and (shape::cstrdefs))])
					else	
						Frame.set_refinement f 
							([], Frame.Qconst [(a, b, Predicate.big_and cstrdefs)])
				else if !(Clflags.reachability) then
					let shape = update_shape_predicates udt_table path cstrname args matche in
					Frame.set_refinement f ([], Frame.Qconst [(a, b, shape)]) 
				else
					Frame.set_refinement f ([], Frame.Qconst [(a, b, Predicate.True)])
			| _ -> assert false
		)
	| (Frame.Fconstr (path, ts, vs, ([], Frame.Qconst qs), eff), _) when (Hashtbl.mem se_env.measures path) -> (
		assert (List.length qs > 0);
		let (q, qs) = (List.hd qs, List.tl qs) in
		match q with 
			| (_,a,Predicate.Atom (b, Predicate.Eq, Predicate.FunApp (cstrname, args))) when Predicate.Var a = b->
				if (Hashtbl.mem se_env.measure_defs (path, cstrname)) then
					let measures = Hashtbl.find se_env.measure_defs (path, cstrname) in
					let cstrdefs = List.map (fun (measure, cstrargs, cstrdef) ->
						let substs = List.map2 (fun cstrarg arg -> (cstrarg, arg)) cstrargs args in
						let substs = (Frame.returnpath, Predicate.FunApp (measure, [Constraint.qual_test_expr]))::substs in
						Predicate.apply_substs substs cstrdef
					) measures in
					if !(Clflags.reachability) then
						let shape = update_shape_predicates udt_table path cstrname args Constraint.qual_test_expr in
						Frame.set_refinement f 
							([], Frame.Qconst ((Path.mk_ident "", Constraint.qual_test_var, Predicate.big_and (shape::cstrdefs))::qs))
					else
						Frame.set_refinement f 
							([], Frame.Qconst ((Path.mk_ident "", Constraint.qual_test_var, Predicate.big_and cstrdefs)::qs))
				else if !(Clflags.reachability) then
						let shape = update_shape_predicates udt_table path cstrname args Constraint.qual_test_expr in
						Frame.set_refinement f 
							([], Frame.Qconst ((Path.mk_ident "", Constraint.qual_test_var, shape)::(qs))) 
				else f		
			| _ -> f
			)
	| _ -> f

let update_measure_env se_env env udt_table = 
	Lightenv.mapi (fun p f -> update_measure_frame se_env udt_table (Some p) f) env

(*lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;*)
let update_measure se_env udt_table cs = 
	List.map (fun c -> match c.lc_orig with
		| Cstr _ -> assert false
		| _ ->
			let cstr = c.lc_cstr in 
			let cstr = match cstr with
				| SubFrame (env, g, f1, f2) -> 
					SubFrame (update_measure_env se_env env udt_table, g, 
						Frame.map (update_measure_frame se_env udt_table None) f1, Frame.map (update_measure_frame se_env udt_table None) f2)
				| WFFrame (env, f) ->
					WFFrame (update_measure_env se_env env udt_table, Frame.map (update_measure_frame se_env udt_table None) f) in
			{lc_cstr = cstr; lc_tenv = c.lc_tenv; lc_orig = c.lc_orig; lc_id = c.lc_id}
	) cs