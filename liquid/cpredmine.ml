(* Mine interesting predciates from test data *)
let count_names constraints = 
	let names = List.fold_left (fun res cnst -> 
		res @ (Predicate.vars cnst)
		) [] constraints in
	Common.remove_duplicates names		
	
let intersect constraints = 
	List.exists (fun c -> 
		List.exists (fun c' -> 
			if c <> c' then 
				let cvars = Predicate.vars c in
				let cvars' = Predicate.vars c' in
				(List.exists (fun cvar -> List.exists (fun cvar' ->
					not (Path.same cvar cvar')
					) cvars') cvars)
				&& (List.exists (fun cvar -> List.exists (fun cvar' ->
					Path.same cvar cvar'
					) cvars') cvars)
			else false
			) constraints
		) constraints	

let transl_samples samples tbl = 
	try List.map (fun sample ->
		List.map (fun (name, value) -> 
			(Hashtbl.find tbl name, Predicate.PInt value)) sample
		) samples with _ -> assert false

let genGtTemplate plains coeffs ds = 
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in 
	Predicate.Atom (t, Predicate.Gt, Predicate.PInt 0)
	
let genGeTemplate plains coeffs ds = 
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in 
	Predicate.Atom (t, Predicate.Ge, Predicate.PInt 0)	
	
let genEqTemplate plains coeffs ds =
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi plain in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in
	Predicate.Atom (t, Predicate.Eq, Predicate.PInt 0)
	
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
	
let exists_negative samples names =
	List.exists (fun name -> 
		List.exists (fun sample -> 
			let d = List.assoc name sample in
			match d with 
				| Predicate.PInt d -> 
					d < 0
				| _ -> assert false
			) samples
		&& List.for_all (fun sample -> 
			let d = List.assoc name sample in
			match d with 
				| Predicate.PInt d -> d <= 0
				| _ -> assert false) samples
		) names
	
let gen_atomic_template names coeffs ds tbl enforces cranges dranges flag dimension samples free = 
	let template = 
		if flag = 1 then genGeTemplate names coeffs ds 
		else if flag = 2 then genEqTemplate names coeffs ds
		else if flag = 3 then genGtTemplate names coeffs ds 
		else assert false in	
	(** Go to the constraint solving system *)
	(** If to find equality invariant, allow more relaxtion only if samples are large *)
	(* We want to further limit coeffs can only be assigned to -1, 0, 1 *)
	let coeff_ranges = 
		if (flag = 2 && List.length samples >= 10 && free) then (** coeffs in equality can bound to freedom *)
			Predicate.big_or (Hashtbl.fold (fun k _ res -> 
			Predicate.big_or (List.flatten (List.map (fun c -> 
			if (c <> 0) then
			[Predicate.Atom (Predicate.PInt (0-c), Predicate.Eq, k); 
			 Predicate.Atom (Predicate.PInt c, Predicate.Eq, k)]
			else []
			) cranges))	::res) coeffs [])
		else Predicate.big_and (Hashtbl.fold (fun k _ res -> 
			Predicate.big_or (List.flatten (List.map (fun c -> 
			[Predicate.Atom (Predicate.PInt (0-c), Predicate.Eq, k); 
			 Predicate.Atom (Predicate.PInt c, Predicate.Eq, k)]
			) cranges))	::res) coeffs []) in
	(* And we want to bound the constants that appears in the template *)
	let d_ranges = 
		if (flag = 2 && List.length samples >= 4) then Predicate.True (** equality can refer to any constants *)
		else Predicate.big_and (Hashtbl.fold (fun d _ res -> 
		Predicate.big_or (List.flatten (List.map (fun drange -> 
			([Predicate.Atom (Predicate.PInt (0-drange),Predicate.Eq,d);
			Predicate.Atom (d,Predicate.Eq,Predicate.PInt drange)]) @ 
			if flag = 1 then [Predicate.Atom (Predicate.PInt (1-drange),Predicate.Eq,d);
			Predicate.Atom (d,Predicate.Eq,Predicate.PInt (drange-1))] else []
			) dranges))::res) ds []) in
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
	let auxcf = 
		if (dimension <= 0) then auxcf 
		else if (flag = 2) then Predicate.And (auxcf, Predicate.Atom (sumaux, Predicate.Eq, Predicate.PInt dimension)) 
		else if dimension <= 2 then 
			Predicate.And (auxcf, Predicate.Atom (sumaux, Predicate.Le, Predicate.PInt dimension)) 
		else Predicate.And (auxcf, Predicate.Atom (sumaux, Predicate.Gt, Predicate.PInt 2)) in
	(* Post-condition requires the existence ofr 'r' *)
	let enforcements = Hashtbl.fold (fun cvar var res ->
		if (List.exists (fun enforce ->
				if (Hashtbl.mem tbl enforce) then 
					Path.same var (Hashtbl.find tbl enforce)
				else false) enforces) then 
				res @ [Predicate.Atom (cvar, Predicate.Ne, Predicate.PInt 0)]
		else res
		) coeffs [] in
	let enforcements = if (List.length enforcements = 0) then (Predicate.True) else Predicate.big_or enforcements in 
	let coeffs = Hashtbl.fold (fun k v res -> res @ [k]) coeffs [] in
	let ds = Hashtbl.fold (fun k v res -> res @ [k]) ds [] in
	(* And we want to constrain at least one of the coeffs is non-zero *)
	let existence = Predicate.big_or (List.fold_left (fun res k -> 
		(Predicate.Atom (k, Predicate.Ne, Predicate.PInt 0))::res	
	) [] coeffs) in
	(* And we want to constrain at least one of the coeffs@ds is positive *)
	let existence' = Predicate.big_or (List.fold_left (fun res k -> 
		(Predicate.Atom (k, Predicate.Gt, Predicate.PInt 0))::res	
	) [] (coeffs@ds)) in
	(* And we want to constrain at least one of the coeffs@ds is negative *)
	let existence'' = 
		if (flag = 1) then
			Predicate.big_or (
				(Predicate.And (auxcf, Predicate.Atom (sumaux, Predicate.Eq, Predicate.PInt 1)))::
			(List.fold_left (fun res k -> 
			(Predicate.Atom (k, Predicate.Lt, Predicate.PInt 0))::res	
			) [] (coeffs@ds))) 
		else if (flag = 2 && exists_negative samples names) then Predicate.True
		else 
			Predicate.big_or (
			(List.fold_left (fun res k -> 
			(Predicate.Atom (k, Predicate.Lt, Predicate.PInt 0))::res	
			) [] (coeffs@ds))) in
	let existences = Predicate.big_and [existence; existence'; existence''] in
	(template, coeff_ranges, d_ranges, enforcements, existences, auxcf)		

(** You can give me = invariants involving any number of variables *)			
let synthesize_eq' flag fpath names pos_samples cranges dranges tbl enforces dimension free = 	
	(** 2) Find forall eq- relations with dimensions more than 2 *)
	let (template, coeffs, ds, cf) = 				
		let coeffs = Hashtbl.create 13 in (* represents coefficients *)
		let ds = Hashtbl.create 3 in (* represents constants *)
		let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
			(* If we work on partitions, we fix dimension = 2 *)
			(*let dimension = if (flag > 1) then 2 else 0 in*)
			gen_atomic_template names coeffs ds tbl enforces cranges dranges 2 dimension pos_samples free in
		(*let constraints = 
			Predicate.big_and ((List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in*)
		let constraints = 
				Predicate.big_and (
					(List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in	
		(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints]) in	
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
	let separators_forall = List.map (fun sep -> simply_coeff sep) separators in		
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "Atomic forall = pred := %a@." (Predicate.pprint') separator	
	) separators_forall in 
	separators_forall
	
(* recursively find all equality information: *)
(* optimization: dimension = 2 should work, otherwise try arbitrary dimensions *)
let rec synthesize_eq flag fpath names pos_samples cranges dranges tbl enforces =
	(*if (flag > 1) then*)
		let result2 = synthesize_eq' flag fpath names pos_samples cranges dranges tbl enforces 2 true in
		let result3 = 
			 	if (List.length (count_names result2) <> List.length names) then
				synthesize_eq' flag fpath names pos_samples cranges dranges tbl enforces 3 (not (intersect result2))
			else [] in
		if ((result2 @ result3) <> []) then result2 @ result3
		else (
			synthesize_eq' flag fpath names pos_samples cranges dranges tbl enforces 0 true)
	(*else synthesize_eq' flag fpath names pos_samples cranges dranges tbl enforces 0*)

(** You can give me >= invariants involving any number of variables *)	
let synthesize_ge flag fpath names pos_samples cranges dranges tbl enforces =
	(** 1) Find forall relations *)
	let _ = Format.fprintf Format.std_formatter "I am working on sythesize_ge@." in
	let (template, coeffs, ds, cf) = 				
		let coeffs = Hashtbl.create 13 in (* represents coefficients *)
		let ds = Hashtbl.create 3 in (* represents constants *)
		let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
			(* If we work on partitions, fix dimension = 2 *)
			let dimension = if (flag > 1) then 2 else 0 in
			gen_atomic_template names coeffs ds tbl enforces cranges dranges 1 dimension pos_samples false in
		(*let constraints = 
			Predicate.big_and ((List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in*)
		let constraints = 
				Predicate.big_and (
					(match template with 
						| Predicate.Atom (e, _, f) -> 
							let t = Predicate.Atom (e, Predicate.Eq, f) in
							Predicate.big_or (List.map (fun pos_sample -> Predicate.apply_substs pos_sample t) pos_samples)
						| _ -> Predicate.True) ::
					(List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in	
		(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints]) in
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
	let separators_forall = List.map (fun sep -> simply_coeff sep) separators in		
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "Atomic forall >= pred := %a@." (Predicate.pprint') separator	
	) separators_forall in separators_forall
	
(** You can give me > invariants involving any number of variables *)	
let synthesize_gt flag fpath names pos_samples cranges dranges tbl enforces = 
	(** 1) Find forall relations *)
	let (template, coeffs, ds, cf) = 				
		let coeffs = Hashtbl.create 13 in (* represents coefficients *)
		let ds = Hashtbl.create 3 in (* represents constants *)
		let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
			(* If we work on partitions, fix dimension = 2 *)
			let dimension = if (flag > 1) then 2 else 0 in
			gen_atomic_template names coeffs ds tbl enforces cranges dranges 3 dimension pos_samples false in
		(*let constraints = 
			Predicate.big_and ((List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in*)
		let constraints = 
				Predicate.big_and (
					(List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in	
		(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints]) in
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
	let separators_forall = List.map (fun sep -> simply_coeff sep) separators in		
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "Atomic forall > pred := %a@." (Predicate.pprint') separator	
	) separators_forall in separators_forall
	
	
(* some variable may always be the same *)
let find_constants tbl samples names =
	if (List.length samples >= 3) then
		let s = List.hd samples in
		let samples = List.tl samples in
		let (cnames, names) = List.partition (fun name -> 
			let name = Path.name name in
			List.for_all (fun sample -> List.assoc name s = List.assoc name sample) samples
			) names in
		(*let _ = List.iter (fun name -> Format.fprintf Format.std_formatter "name = %s@." (Path.name name)) names in*)
		(List.map (fun cname -> 
			(*let _ = Format.fprintf Format.std_formatter "cname = %s@." (Path.name cname) in*)
			Predicate.Atom (Predicate.Var cname, 
				Predicate.Eq, Predicate.PInt (List.assoc (Path.name cname) s))) cnames,
		names)
	else ([], names)		
	
(** A guess of octagon properties from one partition *)	
let synthesize_octagon fpath pos_samples cranges dranges tbl enforces =
	let names = Hashtbl.fold (fun _ name res -> res @ [name]) tbl [] in
	let (constants, names) = find_constants tbl pos_samples names in
	let pos_samples = transl_samples pos_samples tbl in

	let (template, coeffs, ds, cf) = 				
		let coeffs = Hashtbl.create 13 in (* represents coefficients *)
		let ds = Hashtbl.create 3 in (* represents constants *)
		let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
			(* octagon *)
			let dimension = 2 in
			gen_atomic_template names coeffs ds tbl enforces cranges dranges 1 dimension pos_samples false in
		(*let constraints = 
			Predicate.big_and ((List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in*)
		let constraints = 
				Predicate.big_and (
					(match template with 
						| Predicate.Atom (e, _, f) -> 
							let t = Predicate.Atom (e, Predicate.Eq, f) in
							let nt = Predicate.Atom (e, Predicate.Ne, f) in
							Predicate.And (
								Predicate.big_or (List.map (fun pos_sample -> Predicate.apply_substs pos_sample t) pos_samples),
								Predicate.big_or (List.map (fun pos_sample -> Predicate.apply_substs pos_sample nt) pos_samples)
							)
						| _ -> Predicate.True) ::
					(List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in	
		(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints]) in
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
	let separators_forall = List.map (fun sep -> simply_coeff sep) separators in		
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "octagon pred := %a@." (Predicate.pprint') separator	
	) separators_forall in separators_forall	
		 	
			
(** Optimization: 
	1) If a partition can be summarized as = constraints then we are done.
	2) Otherwise, we show whether a partition can be summarized as >= constraints.
	3) If >= constraints are not sufficently sumarizes all variables involved, we get > constraints.
 *)			
(** 1) Find forall equality relation 2) Find existential-eq relation 3) Generalize from existential-eq relation **)			
let synthesize flag fpath pos_samples cranges dranges tbl enforces =
	let names = Hashtbl.fold (fun _ name res -> res @ [name]) tbl [] in
	let (constants, names) = find_constants tbl pos_samples names in
	(*let _ = List.iter (fun name -> Format.fprintf Format.std_formatter "final name = %s@." (Path.name name)) names in*)
	let pos_samples = transl_samples pos_samples tbl in
	(** 1. Find forall = *)
	let sepeq = synthesize_eq flag fpath names pos_samples cranges dranges tbl enforces in
	(** 2. Find forall >= *)
	let sepge = if (sepeq = [] && flag = 1) then synthesize_ge flag fpath names pos_samples cranges dranges tbl enforces else [] in
	(** 3. Find forall > *)
	let sepgt = if (sepeq = [] && List.length (count_names sepge) <> List.length names && flag = 1) 
							then synthesize_gt flag fpath names pos_samples cranges dranges tbl enforces else [] in
	(constants) @ sepeq @ sepge @ sepgt (*@ separators_ex	*)
	(** Generalize from existential-eq relations *)

let synthesize_datatype_constraints fpath pos_samples cranges dranges tbl enforces = 
	let names = Hashtbl.fold (fun _ name res -> res @ [name]) tbl [] in
	let (constants, names) = find_constants tbl pos_samples names in
	let pos_samples = transl_samples pos_samples tbl in 
	let sepeq = synthesize_eq 100 fpath names pos_samples cranges dranges tbl enforces in
	let sepeq = List.flatten (List.map (fun sep -> match sep with
		| Predicate.Atom (a,_,b) -> [Predicate.Atom (a, Predicate.Ge, b); Predicate.Atom (a, Predicate.Le, b)] 
		| _ -> assert false) sepeq) in
	let seps = if (sepeq = []) then synthesize_ge 100 fpath names pos_samples cranges dranges tbl enforces else [] in
	constants @ sepeq @ seps	
					
(** flag = 1 => Find forall >= relation 
		flag = 2 => Find existential = relation 
		flag = 3 => Find existential > relation*)	
(*let synthesize flag pos_samples dranges tbl enforces = 
	let pos_samples = transl_samples pos_samples tbl in
	let names = Hashtbl.fold (fun _ name res -> res @ [name]) tbl [] in
	let (template, coeffs, ds, cf) = 
		if (flag = 1) then 
			let coeffs = Hashtbl.create 13 in (* represents coefficients *)
			let ds = Hashtbl.create 3 in (* represents constants *)
			let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
				(* Fixme. drange can be as great as 3 ? *)
				gen_atomic_template names coeffs ds tbl enforces dranges flag in
			let constraints = 
				Predicate.big_and (
					(match template with 
						| Predicate.Atom (e, _, f) -> 
							let t = Predicate.Atom (e, Predicate.Eq, f) in
							Predicate.big_or (List.map (fun pos_sample -> Predicate.apply_substs pos_sample t) pos_samples)
						| _ -> Predicate.True) ::
					(List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples)) in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints])
		else if (flag = 2 || flag = 3) then 
			let coeffs = Hashtbl.create 13 in (* represents coefficients *)
			let ds = Hashtbl.create 3 in (* represents constants *)
			let (template, coeff_ranges, d_ranges, enforcements, existences, auxcf) = 
				(* Fixme. drange can be as great as 3 ? *)
				gen_atomic_template names coeffs ds tbl enforces dranges flag in
			let constraints = 
				Predicate.big_or (
					List.map (fun pos_sample -> Predicate.apply_substs pos_sample template) pos_samples) in
			(template, coeffs, ds, Predicate.big_and [coeff_ranges; d_ranges; enforcements; existences; auxcf; constraints])
		else assert false in
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
		Format.fprintf Format.std_formatter "Atomic pred = %a@." (Predicate.pprint') separator	
	) separators in
	(separators) *)