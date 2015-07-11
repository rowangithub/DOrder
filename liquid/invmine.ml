let transl_samples samples tbl = 
	try List.map (fun sample ->
		List.map (fun (name, value) -> 
			(Hashtbl.find tbl name, Predicate.PInt value)) sample
		) samples with _ -> assert false

let genTemplate plains coeffs ds flag = 
	if flag then
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in t
	(*Predicate.Atom (t, Predicate.Le, Predicate.PInt 0)*)
	else 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (Predicate.PInt 0, 0) plains in t	
	(*Predicate.Atom (t, Predicate.Lt, Predicate.PInt 0)*)
	
let genEqualTemplate plains coeffs ds =
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in t
	(*Predicate.Atom (t, Predicate.Eq, Predicate.PInt 0)*)
	
(** Try to return all solutions *)
let rec allsolution solutions constraint_f cvars = 
	(*let constraint_f = Predicate.And (constraint_f, Atom (sumpexpr, Eq, PInt k)) in*)
	let restriction = List.fold_left (fun res solution -> 
		let l = List.fold_left (fun res cvar -> 
			let value = 
				try Hashtbl.find solution cvar with _ -> assert false in
			res@[Predicate.Atom (Predicate.Var cvar, Predicate.Ne, Predicate.PInt value)]
		) [] cvars in
		(assert (List.length l > 0); res @ [Predicate.big_or l])
	) [] solutions in
	let _ = assert (List.length restriction > 0) in
	let constraint_f = Predicate.And (constraint_f, Predicate.big_and restriction) in
	let newsolution = TheoremProver.model constraint_f 1 in
	if (List.length newsolution = 0) then solutions
	else 
		let new_solution = List.hd newsolution in
		(*let _ = List.iter (fun cvar -> 
			let value = try Hashtbl.find new_solution cvar with _ -> assert false in
			Format.fprintf Format.std_formatter "%s: %d " (Path.unique_name cvar) value
		) cvars in
		let _ = Format.fprintf Format.std_formatter "@.----@." in*)
		if (List.length solutions > 30) then (solutions@[new_solution])
		else allsolution (solutions@[new_solution]) constraint_f cvars	
		
let simply_coeff pred = 
	Predicate.map_expr (fun expr -> match expr with
		| Predicate.Binop (e1, op, e2) when op = Predicate.Plus -> (match (e1, e2) with
			| (Predicate.PInt 0, _) -> e2
			| (_, Predicate.PInt 0) -> e1
			| (_, Predicate.Binop (Predicate.PInt x,Predicate.Times,e3)) when x = -1 -> 
				Predicate.Binop (e1, Predicate.Minus, e3)
			| _ -> expr)
		| Predicate.Binop (e1, op, e2) when op = Predicate.Minus -> (match e2 with
			| Predicate.PInt 0 -> e1
			| _ -> expr) 
		| Predicate.Binop (e1, op, e2) when op = Predicate.Times -> (match (e1, e2) with
			| (Predicate.PInt 0, _) -> Predicate.PInt 0
			| (_, Predicate.PInt 0) -> Predicate.PInt 0
			| (Predicate.PInt 1, _) -> e2
			| (_, Predicate.PInt 1) -> e1
			| _ -> expr)
		| Predicate.Binop (e1, op, e2) when op = Predicate.Div -> (match e2 with
			| Predicate.PInt 1 -> e1
			| _ -> expr)
		| _ -> expr
	)	pred		
	
(* dranges must be no less than 0 *)
let gen_atomic_template names coeffs ds tbl enforces drange flag n = 
	let template = 
		if (n = 0) then genEqualTemplate names coeffs ds 
		else genTemplate names coeffs ds flag in	
	
	(** Go to the constraint solving system *)
	(* We want to further limit coeffs can only be assigned to -1, 0, 1 *)
	let coeff_ranges = Predicate.big_and (Hashtbl.fold (fun k _ res -> 
		Predicate.And (
			Predicate.Atom (Predicate.PInt (0-1),Predicate.Le,k), 
			Predicate.Atom (k,Predicate.Le,Predicate.PInt 1))::res) coeffs []) in
	(* And we want to bound the constants that appears in the template *)
	let d_ranges = Predicate.big_and (Hashtbl.fold (fun d _ res -> 
		Predicate.And (
			Predicate.Atom (Predicate.PInt (0-drange),Predicate.Le,d),
			Predicate.Atom (d,Predicate.Le,Predicate.PInt drange))::res) ds []) in
	(*let existence = Predicate.big_or (Hashtbl.fold (fun k _ res -> 
		(Predicate.Atom (k, Predicate.Ne, Predicate.PInt 0))::res	
	) coeffs []) in*)		
	(* And we want to constrain at least one of the coeffs is postive *)
	let existence' = Predicate.big_or (Hashtbl.fold (fun k _ res -> 
		(Predicate.Atom (k, Predicate.Gt, Predicate.PInt 0))::res	
	) coeffs []) in
	(* And we want to constrain at least one of the coeffs is negative *)
	let existence'' = Predicate.big_or (Hashtbl.fold (fun k _ res -> 
		(Predicate.Atom (k, Predicate.Lt, Predicate.PInt 0))::res	
	) coeffs []) in
	let existence = Predicate.And (existence', existence'') in
	(* And we want to constraint at most two of the coeffs are nonzero *)
	let auxvars = Hashtbl.fold (fun k _ res -> res@[(Predicate.Var (Path.mk_ident "a"), k)]) coeffs [] in 
	let sumaux = List.fold_left (fun res (a, _) -> 
		Predicate.Binop (res, Predicate.Plus, a)
	) (fst (List.hd auxvars)) (List.tl auxvars) in
	let auxcf = Predicate.big_and (List.map (fun (a, c) -> 
		Predicate.Or (
			Predicate.And (
				Predicate.Atom (a, Predicate.Eq, Predicate.PInt 0), 
				Predicate.Atom (c, Predicate.Eq, Predicate.PInt 0)), 
			Predicate.And (
				Predicate.Atom (a, Predicate.Eq, Predicate.PInt 1), 
				Predicate.Atom (c, Predicate.Ne, Predicate.PInt 0)))	
	) auxvars) in
	let auxcf = Predicate.And (auxcf, Predicate.Atom (sumaux, Predicate.Eq, Predicate.PInt 2)) in
	
	(* Post-condition requires the existence ofr 'r' *)
	let enforcements = Hashtbl.fold (fun cvar var res ->
		if (List.exists (fun enforce ->
				if (Hashtbl.mem tbl enforce) then 
					Path.same var (Hashtbl.find tbl enforce)
				else false) enforces) then 
			if (n = 0) then res @ [Predicate.Atom (cvar, Predicate.Gt, Predicate.PInt 0)]
			else res @ [Predicate.Atom (cvar, Predicate.Ne, Predicate.PInt 0)]
		else res
		) coeffs [] in
	let enforcements = if (List.length enforcements = 0) then (Predicate.True) else Predicate.big_or enforcements in 
	(*let _ = Format.fprintf Format.std_formatter "enforcements = %a@." Predicate.pprint enforcements in		*)
	(template, coeff_ranges, d_ranges, existence, enforcements, auxcf)
	


exception LimitExpresivity
						
(* Generate a general template and instantiate it with concrete program state*)
(* If this is called even neg_samples is empty, we gradually increase the experssivity of the templates *)
(* We use drange to limit the constants used in the invariant *)
let learn' n pos_samples neg_samples drange tbl enforces env fr = 
	try (*let _ = List.iter (fun p ->
		let _ = Format.fprintf Format.std_formatter "pos" in
		let _ = List.iter (fun (n, v) -> 
		Format.fprintf Format.std_formatter " %s:%d " n v) p in
		Format.fprintf Format.std_formatter "\n"
		) pos_samples in
	let _ = List.iter (fun n ->
		let _ = Format.fprintf Format.std_formatter "neg" in
		let _ = List.iter (fun (n, v) ->  
		Format.fprintf Format.std_formatter " %s:%d " n v) n in
		Format.fprintf Format.std_formatter "\n"
		) neg_samples in*)
	(** names are better with paths in the favor of the implementation *)
	let pos_samples = transl_samples pos_samples tbl in
	let neg_samples = transl_samples neg_samples tbl in
	
	let names = Hashtbl.fold (fun _ name res -> res @ [name]) tbl [] in
	
	(* Build the constraints: we consider template like A \/ B \/ C ... *)
	(*let _ = Format.fprintf Format.std_formatter "N = %d@." n in*)
	let (template, coeffs, ds, cf) = 
		if (n = 0) then 
			let coeffs = Hashtbl.create 13 in (* represents coefficients *)
			let ds = Hashtbl.create 3 in (* represents constants *)
			let (template, coeff_ranges, d_ranges, existence, enforcements, auxcf) = 
				(* Fixme. drange can be as great as 3 ? *)
				gen_atomic_template names coeffs ds tbl enforces drange true n in
			let template = Predicate.Atom (template, Predicate.Eq, Predicate.PInt 0) in	
			let constraints = Predicate.And (
				Predicate.big_and (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples), 
				Predicate.big_and (
					List.map (fun neg_sample -> Predicate.Not (Predicate.apply_substs neg_sample template)) neg_samples)) in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; existence; enforcements; constraints])
		else if (n = 1) then 
			let coeffs = Hashtbl.create 13 in (* represents coefficients *)
			let ds = Hashtbl.create 3 in (* represents constants *)
			let (template, coeff_ranges, d_ranges, existence, enforcements, auxcf) = 
				(* Fixme. drange can be as great as 3 ? *)
				gen_atomic_template names coeffs ds tbl enforces drange true n in
			(*let template' = Predicate.Atom (template, Predicate.Eq, Predicate.PInt 0) in*)
			let template = Predicate.Atom (template, Predicate.Le, Predicate.PInt 0) in
			let constraints = Predicate.And (
				Predicate.big_and (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples), 
				Predicate.big_and (
					List.map (fun neg_sample -> Predicate.Not (Predicate.apply_substs neg_sample template)) neg_samples)) in
			(*let constraints' = 
				Predicate.big_or (List.map (fun pos_sample -> Predicate.apply_substs pos_sample template') pos_samples) in
			let constraints = Predicate.And (constraints, constraints') in		*)
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; existence; enforcements; auxcf; constraints])
		else if (n = 2 && List.length names > 2) then
			let coeffs1 = Hashtbl.create 13 in (* represents coefficients *)
			let ds1 = Hashtbl.create 3 in (* represents constants *)
			let (template1, coeff_ranges1, d_ranges1, existence1, enforcements1, auxcf1) = 
				(* Fixme. drange is controlled to 1 ? *)
				gen_atomic_template names coeffs1 ds1 tbl enforces drange false n in
			let template1 = Predicate.Atom (template1, Predicate.Lt, Predicate.PInt 0) in	
			let coeffs2 = Hashtbl.create 13 in (* represents coefficients *)
			let ds2 = Hashtbl.create 3 in (* represents constants *)
			let (template2, coeff_ranges2, d_ranges2, existence2, enforcements2, auxcf2) = 
				(* Fixme. drange is controlled to 1? *)
				gen_atomic_template names coeffs2 ds2 tbl enforces drange true n in 
			let template2 = Predicate.Atom (template2, Predicate.Le, Predicate.PInt 0) in
			(* Some postive samples should not be covered by A *)
			let uncover1 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template1)) pos_samples) in
			(* Some postive sampels should not be covered by B *)
			let uncover2 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template2)) pos_samples) in
			(* However, A \/ B should cover all samples *)
			let template = Predicate.Or (template1, template2) in
			let constraints = Predicate.And (
				Predicate.big_and (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples), 
				Predicate.big_and (
					List.map (fun neg_sample -> Predicate.Not (Predicate.apply_substs neg_sample template)) neg_samples)) in
			(* We optimize A and B should not share the same coeffs *)
			let cm1 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs1 [] in
			let cm2 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs2 [] in
			let cm12 = Predicate.Not (Predicate.big_and (List.fold_left (fun res (p1, c1) -> 
				let c2 = List.assoc p1 cm2 in
				res @ [Predicate.Atom (Predicate.Binop (c1, Predicate.Plus, c2), Predicate.Eq, Predicate.PInt 0)] 	
			) [] cm1)) in
			(* Make the constraint *)
			let coeffs = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) coeffs2 coeffs1 in		
			let ds = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) ds2 ds1 in	
			let coeff_ranges = Predicate.And (coeff_ranges1, coeff_ranges2) in
			let d_ranges = Predicate.And (d_ranges1, d_ranges2) in
			let existence = Predicate.And (existence1, existence2) in
			let enforcements = Predicate.Or (enforcements1, enforcements2) in
			let auxcf = Predicate.And (auxcf1, auxcf2) in
			let auxcf = Predicate.And (cm12, auxcf) in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; existence; enforcements; auxcf; uncover1; uncover2; constraints])
		(*else if (n = 1 && List.length names > 2) then
			let coeffs1 = Hashtbl.create 13 in (* represents coefficients *)
			let ds1 = Hashtbl.create 3 in (* represents constants *)
			let (template1, coeff_ranges1, d_ranges1, existence1, enforcements1, auxcf1) = gen_atomic_template names coeffs1 ds1 tbl enforces true in
			let coeffs2 = Hashtbl.create 13 in (* represents coefficients *)
			let ds2 = Hashtbl.create 3 in (* represents constants *)
			let (template2, coeff_ranges2, d_ranges2, existence2, enforcements2, auxcf2) = gen_atomic_template names coeffs2 ds2 tbl enforces false in 
			(* Some postive samples should not be covered by A *)
			let uncover1 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template1)) pos_samples) in
			(* Some postive sampels should not be covered by B *)
			let uncover2 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template2)) pos_samples) in
			(* However, A \/ B should cover all samples *)
			let template = Predicate.Or (template1, template2) in
			let constraints = Predicate.And (
				Predicate.big_and (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples), 
				Predicate.big_and (
					List.map (fun neg_sample -> Predicate.Not (Predicate.apply_substs neg_sample template)) neg_samples)) in
			(* We optimize A and B should not share the same coeffs *)
			let cm1 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs1 [] in
			let cm2 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs2 [] in
			let cm12 = Predicate.Not (Predicate.big_and (List.fold_left (fun res (p1, c1) -> 
				let c2 = List.assoc p1 cm2 in
				res @ [Predicate.Atom (Predicate.Binop (c1, Predicate.Plus, c2), Predicate.Eq, Predicate.PInt 0)] 	
			) [] cm1)) in
			(* Make the constraint *)
			let coeffs = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) coeffs2 coeffs1 in		
			let ds = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) ds2 ds1 in	
			let coeff_ranges = Predicate.And (coeff_ranges1, coeff_ranges2) in
			let d_ranges = Predicate.And (d_ranges1, d_ranges2) in
			let existence = Predicate.And (existence1, existence2) in
			let enforcements = Predicate.Or (enforcements1, enforcements2) in
			let auxcf = Predicate.And (auxcf1, auxcf2) in
			let auxcf = Predicate.And (cm12, auxcf) in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; existence; enforcements; auxcf; uncover1; uncover2; constraints])*)
		(*else if (n = 2 && List.length names > 2) then
			let coeffs1 = Hashtbl.create 13 in (* represents coefficients *)
			let ds1 = Hashtbl.create 3 in (* represents constants *)
			let (template1, coeff_ranges1, d_ranges1, existence1, enforcements1, auxcf1) = gen_atomic_template names coeffs1 ds1 tbl enforces true in
			let coeffs2 = Hashtbl.create 13 in (* represents coefficients *)
			let ds2 = Hashtbl.create 3 in (* represents constants *)
			let (template2, coeff_ranges2, d_ranges2, existence2, enforcements2, auxcf2) = gen_atomic_template names coeffs2 ds2 tbl enforces true in 
			let coeffs3 = Hashtbl.create 13 in (* represents coefficients *)
			let ds3 = Hashtbl.create 3 in (* represents constants *) 
			let (template3, coeff_ranges3, d_ranges3, existence3, enforcements3, auxcf3) = gen_atomic_template names coeffs3 ds3 tbl enforces true in 
			(* Some postive samples should not be covered by A *)
			let uncover1 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template1)) pos_samples) in
			(* Some postive samples should not be covered by B *)
			let uncover2 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template2)) pos_samples) in
			(* Some postive samples should not be covered by C *)
			let uncover3 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample template3)) pos_samples) in
			(* Some postive samples should not be covered by A \/ B *)
			let uncover12 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample (Predicate.And (template1, template2)))) pos_samples) in
			(* Some postive samples should not be covered by B \/ C*)
			let uncover23 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample (Predicate.And (template2, template3)))) pos_samples) in
			(* Some postive samples should not be covered by A \/ C*)	
			let uncover13 = Predicate.big_or (List.map (fun pos_sample -> 
				Predicate.Not (Predicate.apply_substs pos_sample (Predicate.And (template1, template3)))) pos_samples) in
			(* However, A \/ B \/ C should cover all samples *)
			let template = Predicate.big_or [template1; template2; template3] in
			let constraints = Predicate.And (
				Predicate.big_and (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples), 
				Predicate.big_and (
					List.map (fun neg_sample -> Predicate.Not (Predicate.apply_substs neg_sample template)) neg_samples)) in
			(* We optimize A, B and C should not share the complementary coeffs *)
			let cm1 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs1 [] in
			let cm2 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs2 [] in
			let cm3 = Hashtbl.fold (fun k v res -> res @ [(v, k)]) coeffs3 [] in
			let cm12 = Predicate.Not (Predicate.big_and (List.fold_left (fun res (p1, c1) -> 
				let c2 = List.assoc p1 cm2 in
				res @ [Predicate.Atom (Predicate.Binop (c1, Predicate.Plus, c2), Predicate.Eq, Predicate.PInt 0)] 	
			) [] cm1)) in
			let cm23 = Predicate.Not (Predicate.big_and (List.fold_left (fun res (p2, c2) -> 
				let c3 = List.assoc p2 cm3 in
				res @ [Predicate.Atom (Predicate.Binop (c2, Predicate.Plus, c3), Predicate.Eq, Predicate.PInt 0)] 	
			) [] cm2)) in
			let cm13 = Predicate.Not (Predicate.big_and (List.fold_left (fun res (p1, c1) -> 
				let c3 = List.assoc p1 cm3 in
				res @ [Predicate.Atom (Predicate.Binop (c1, Predicate.Plus, c3), Predicate.Eq, Predicate.PInt 0)] 	
			) [] cm1)) in
			(* Make the constraint *)
			let coeffs = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) coeffs2 coeffs1 in
			let coeffs = Hashtbl.fold (fun k3 v3 h12 -> (Hashtbl.replace h12 k3 v3; h12)) coeffs3 coeffs in 		
			let ds = Hashtbl.fold (fun k2 v2 h1 -> (Hashtbl.replace h1 k2 v2; h1)) ds2 ds1 in	
			let ds = Hashtbl.fold (fun k3 v3 h12 -> (Hashtbl.replace h12 k3 v3; h12)) ds3 ds in
			let coeff_ranges = Predicate.And (coeff_ranges1, coeff_ranges2) in
			let coeff_ranges = Predicate.And (coeff_ranges3, coeff_ranges) in
			let d_ranges = Predicate.And (d_ranges1, d_ranges2) in
			let d_ranges = Predicate.And (d_ranges3, d_ranges) in
			let existence = Predicate.And (existence1, existence2) in
			let existence = Predicate.And (existence3, existence) in
			let enforcements = Predicate.Or (enforcements1, enforcements2) in
			let enforcements = Predicate.Or (enforcements3, enforcements) in
			let auxcf = Predicate.And (auxcf1, auxcf2) in
			let auxcf = Predicate.And (auxcf3, auxcf) in
			let auxcf = Predicate.And (Predicate.big_and [cm12; cm23; cm13], auxcf) in
			let uncover = Predicate.big_and [uncover1; uncover2; uncover3; uncover12; uncover23; uncover13] in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; existence; enforcements; auxcf; uncover; constraints])*)
		else 
			(Format.fprintf Format.std_formatter "Templates must require more disjunctions!!@."; raise LimitExpresivity) in
	
	(*let _ = Format.fprintf Format.std_formatter "cf = %a" Predicate.pprint cf in*)
	let solutions = TheoremProver.model cf 1 in
	let separators = 
		if (List.length solutions = 0) then []
		else 
			let solution = List.hd solutions in
			let coeffs = Hashtbl.fold (fun k _ res -> match k with Predicate.Var k -> k::res | _ -> assert false) coeffs [] in
			let ds = Hashtbl.fold (fun d _ res -> match d with Predicate.Var d -> d::res | _ -> assert false) ds [] in
			let solutions = allsolution [solution] cf (coeffs@ds) in
			List.map (fun solution -> 
				let solution = Hashtbl.fold (fun p i res -> res@[(p, Predicate.PInt i)]) solution [] in
				Predicate.apply_substs solution template
			) solutions in
	let separators = List.map (fun sep -> simply_coeff sep) separators in		
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "Separator = %a@." (Predicate.pprint') separator	
	) separators in
	(List.fold_left (fun res sep -> match sep with
		| Predicate.Atom (a1, Predicate.Eq, a2) -> 
			res @ [Predicate.Atom (a1, Predicate.Le, a2); Predicate.Atom (a1, Predicate.Ge, a2)]
		| sep -> res @ [sep]) [] separators)
	with LimitExpresivity -> 
		(Format.fprintf Format.std_formatter "Learning algorithm cannot provide any hint when drange is %d@." drange; ( []))
	
let rec learn n pos_samples neg_samples tbl enforces env fr = 
	let rec loop drange res = 
		if (drange <= 3) then 
			let (res) = learn' n pos_samples neg_samples drange tbl enforces env fr in
			if (res = []) then loop (drange+2) res
			else res
			(*if (flag) then res
			else loop (drange+2) res*)
		else (Format.fprintf Format.std_formatter "Learning algorithm cannot provide any hint@."; res) in
	let pre_res = 
		(if n = 2 then (learn (n-1) pos_samples neg_samples tbl enforces env fr) else []) in
	(loop 1 []) @ pre_res