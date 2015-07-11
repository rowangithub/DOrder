module TP = TheoremProver

open Backwalker
open Predicate

type solving_result = {
	spre : ((string * int) list) list; 
	spost : ((string * int) list) list 
}

(** This defines, for each variable, how many solutions of it should be consider  *)
let neg_number = 2

(** If the function app is not with enough arguments to deduce a non-function value,
 	  the app should be erased *)
let simplify_funapp pred allbindings = 
	let hashfuns = Hashtbl.create 1 in
	Predicate.map_expr (fun pexpr -> match pexpr with
		| FunApp (fn, es) ->  
			if (String.compare fn "UF" = 0) then
				let (hd, tl) = (List.hd es, List.tl es) in
				match hd with
					| Var funvar -> 
						(try
							let (_,fr) = List.find (fun (p,f) -> Path.same p funvar) allbindings in
							let n_args = Frame.count_args fr in
							let n_actuals = (List.length es)-1 in
							if (n_actuals >= n_args) then
								(FunApp (Path.name funvar, tl))
							else (** put a dummy here *) (Predicate.Var (Path.mk_ident "dummy"))
						with _ -> 
							(** Fixme. This function is unknown. Put a dummy here *)
							if (Hashtbl.mem hashfuns pexpr) then Hashtbl.find hashfuns pexpr
							else 
								let f = Predicate.Var (Path.mk_ident "dummy") in
								let _ = Hashtbl.replace hashfuns pexpr f in f	
						)
					| _ -> (** Fixme? *) assert false
			else pexpr
		| _ -> pexpr
		) pred

(* Precondition: we assume there is no further function application inside an application
   This is guaranteed by our dealing with badformula *)
let destruct_funapp funapp = match funapp with
	| FunApp (fn, es) -> funapp::(List.fold_left (fun res pe -> match pe with
			| Binop (e1, rel, e2) -> res @ [pe]
			| _ -> res		
		) [] es) 
	| _ -> assert false	

let rec pexpr_to_string pexpr = match pexpr with
	| PInt e' -> (string_of_int e')
	| Var e' -> (Path.name e') 
	| FunApp (fn, es) -> 
		List.fold_left (fun res e -> match e with
			| Var _ -> (res ^ "_" ^ (pexpr_to_string e))
			| PInt _ -> (res ^ "_" ^ (pexpr_to_string e))
			| Binop _ -> (res ^ "_" ^ (pexpr_to_string e))
			| _ -> assert false
			) fn es
	| Binop (e1, rel, e2) -> ((pexpr_to_string e1) ^ (pprint_op rel) ^ (pexpr_to_string e2))
	| _ -> assert false

let getFunApp pred = 
	let funapps = ref [] in
	(ignore (Predicate.map_expr (fun pexpr -> match pexpr with
		| FunApp (fn, es) -> (** Fixme? *)
			let pexprs = destruct_funapp pexpr in
			let pexprs = List.map (fun pexpr -> 
				equals (pexpr,
				(Var (Path.Pident (Ident.create_persistent (pexpr_to_string pexpr)))))	
			) pexprs in
			((funapps := pexprs
				@ (!funapps)); pexpr)
		| _ -> pexpr
		) pred);
	(!funapps))
			
exception Out_of_loop

let rec permutations l =
	let n = List.length l in
	if n = 1 then [l] else
  let rec sub e = function
  	| [] -> failwith "sub"
    | h :: t -> if h = e then t else h :: sub e t in
  let rec aux k =
  	let e = List.nth l k in
    let subperms = permutations (sub e l) in
    let t = List.map (fun a -> e::a) subperms in
    if k < n-1 then List.rev_append t (aux (k+1)) else t in
	aux 0;; 

(** Iteratively return a set of solutions *)		
let iter_solve pred neg_number = 
	(*let _ = Format.fprintf Format.std_formatter "The bad formula is %a@." Predicate.pprint' pred in*)
	TP.model pred 1 
	(*let vars = Predicate.vars pred in
	let vars = Common.remove_duplicates vars in
	let perms = permutations vars in
	let solutions = ref [] in
	let _ = List.iter (fun vars -> 
		let restriction = List.fold_left (fun (prev, res) curr -> 
			(curr, Predicate.And (res, Predicate.Atom (Predicate.Var prev, Predicate.Le, Predicate.Var curr)))
			) (List.hd vars, Predicate.True) (List.tl vars) in
		let restriction = snd restriction in
		let pred = Predicate.And (restriction, pred) in
		let model = TP.model pred 1 in
			if (List.length model = 0) then ()
			else 
				let new_assignment = List.hd model in
				(*let _ = Hashtbl.iter (fun path value -> 
					Format.fprintf Format.std_formatter "path:value %s:%d" (Path.name path) value
					) new_assignment in
				let _ = Format.fprintf Format.std_formatter "@." in*)
				(solutions := new_assignment::(!solutions))
	) perms in
	Common.remove_duplicates (!solutions)*)
	
(*let iter_solve pred neg_number = 
	(** generate the initial solution  *)
	let model = TP.model pred 1 in
	if (List.length model = 0) then []
	else 
		let assignment = List.hd model in
		let vars = Predicate.vars pred in
		let vars = Common.remove_duplicates vars in
		let n = List.length vars in
		let solutions = ref ([assignment]) in
		let rec solve i fixed assignment = 
			if (i < n) then
				let currassn = ref [] in
				let assignment = ref assignment in
				for j = 1 to neg_number do
					(** acquire and store the solution for i *)
					try (
					let var = List.nth vars i in
					let _ = Format.fprintf Format.std_formatter "solving on var as %s @." (Path.name var) in
					let value = (try Hashtbl.find (!assignment) var with _ -> assert false) in
					let _ = Format.fprintf Format.std_formatter "value as %d@." value in
					let fixed' = (var, Predicate.PInt value)::fixed in
					let _ = (currassn := (value::(!currassn))) in
					(** work for i+1 level *)
					let _ = solve (i+1) fixed' (!assignment) in
					(** make the current pred by applying fixed 
						* and exclude former assignment to this level*)
					let pred = Predicate.apply_substs fixed pred in
					let excludes = List.map (fun exclude -> 
						Predicate.Atom (Predicate.Var var, Predicate.Ne, Predicate.PInt exclude)
						) (!currassn) in
					let pred = Predicate.big_and (pred::excludes) in
					let model = TP.model pred 1 in
					if (List.length model = 0) then (
						Format.fprintf Format.std_formatter "Cannot Solve %a @." Predicate.pprint pred;
						raise Out_of_loop)
					else 
						let new_assignment = List.hd model in
						(** fixed + new_assignment *)
						let _ = List.iter (fun (path, value) -> 
							match value with
								| Predicate.PInt iv -> Hashtbl.replace new_assignment path iv 
								| _ -> assert false 
							) fixed in
						(*let _ = Hashtbl.iter (fun path value -> 
							Format.fprintf Format.std_formatter "path:value %s:%d" (Path.name path) value
							) new_assignment in
						let _ = Format.fprintf Format.std_formatter "@." in*)
						(solutions := new_assignment::(!solutions); assignment := new_assignment)
					) with _ -> ()
					(** go to for loop to repeat in order to acquire and store new solution for i *)
				done;
			else () in
	let _ = solve 0 [] assignment in
	(!solutions)*)
let simple_arr pexpr allbindings = match pexpr with
	| FunApp (fn, es) when (String.compare fn "UF" = 0) ->
		(try 
			let arr = List.hd es in
			let arr = match arr with Predicate.Var arr -> arr | _ -> assert false in
			match (List.find (fun (p,f) -> Path.same p arr) allbindings) with
				| (_, Frame.Fconstr (x,_,_,_,_)) when (x = Predef.path_array) ->
					FunApp (Path.name arr, List.tl es)
				| _ -> assert false
		with _ -> assert false)
	| _ -> assert false	

(** simple validation:
     A negative sample could, however, make the bad formula UNSAT *)
let check_neg_sample bad samples = 
	(*try*) List.filter (fun sample -> 
		let (hos, scalars) = List.partition (fun (s, v) -> 
			Common.str_contains s "_r"
		) sample in
		(* Translate the solution into a precondition *)
		let vars = Predicate.vars bad in
		let hospre = List.map (fun (ho, v) -> 
			let ho = String.sub ho 0 (String.length ho - 2) in
			let hoparams = try List.find_all (fun (s, _) -> 
				Common.str_contains s ho	
			) scalars with _ -> assert false in	
			let hoparams = List.map (fun (_, v) -> Predicate.PInt v) hoparams in
			Predicate.Atom (Predicate.FunApp (ho, hoparams), Predicate.Eq, Predicate.PInt v)
		) hos in
		let scalarpre = Common.map_partial (fun (s, v) -> 
			try 
				let var = List.find (fun var -> String.compare (Path.name var) s = 0) vars in 
				Some (Predicate.Atom (Predicate.Var var, Predicate.Eq, Predicate.PInt v))
			with _ -> None 
		) scalars in
		(* Check whether the solution can really imply the bad condition *)
		let result = TheoremProver.implies (Predicate.big_and (hospre@scalarpre)) bad in
		(TheoremProver.finish (); 
		if (not result) then (Format.fprintf Format.std_formatter "check: %a error@." 
			Predicate.pprint (Predicate.implies (Predicate.big_and (hospre@scalarpre), bad))); 
		result)
	) samples (*with _ -> 
		(Format.fprintf Format.std_formatter "Internal checking bad sample error@."; assert false)*)

let solve dty env funframebindings badbindings unsounds samples = 
	let generate path restriction bad postbad = 
		let funfr = Hashtbl.find funframebindings path in
		let allbindings = Frame.get_fun_bindings env funfr in
		let unsounds = if dty then [] else Hashtbl.find unsounds path in
		(*let _ = List.iter (fun u -> Format.fprintf Format.std_formatter "Unsound %a@." Predicate.pprint_pexpr u) unsounds in*)
		(* We want to care about for-all array property *)
		let arr_pair = Backwalker.detect_arr_adj_pattern postbad allbindings unsounds in
		(*let _ = Format.fprintf Format.std_formatter "ARRPAIR:@." in
		let _ = List.iter (fun (p, p') -> Format.fprintf Format.std_formatter "(%a, %a)@." 
						Predicate.pprint_pexpr p Predicate.pprint_pexpr p') arr_pair in*)
		let bad = Predicate.And (restriction, bad) in
		let bad_constraint = simplify_funapp bad allbindings in
		let badpred = big_and (bad_constraint :: (getFunApp bad_constraint)) in 
		let bad_assignments = iter_solve badpred neg_number in
	
		let transl_assignments assignments bad_constraint = 
			(List.flatten (List.map (fun assignment -> 
			(*let store = ref [] in*)
			let plains = Hashtbl.create 5 in
			let hos = Hashtbl.create 5 in
			(* We want to deal with each uninterpreted function only once *)
			let ufs = Hashtbl.create 5 in
			(ignore (Predicate.map_expr (fun pexpr -> match pexpr with
				| Predicate.Var path -> 
					(*let value = (try Hashtbl.find assignment path with _ -> assert false) in
					(Hashtbl.replace plains (Path.name path) [[(Path.name path, value)]]; pexpr)*)
					(try 
						let value = Hashtbl.find assignment path in
						(Hashtbl.replace plains (Path.name path) [[(Path.name path, value)]]; pexpr)
					with _ -> pexpr)
				| Predicate.FunApp (fn, es) -> 
					if (Hashtbl.mem ufs pexpr) then pexpr else (* unsound application is filtered out *)
					if (List.exists (fun u -> pexpr = u) unsounds) then pexpr else
					(* if this funapp appears in array adj dection, specially encode it *)
					if (List.exists (fun (_, pe) -> (simple_arr pe allbindings) = pexpr) arr_pair) then pexpr else
					if (List.exists (fun (pe, _) -> (simple_arr pe allbindings) = pexpr) arr_pair) then
					(* dumping paired array loggings *)
						let (_, pexpr') = List.find (fun (pe, _) -> (simple_arr pe allbindings) = pexpr) arr_pair in
						let pexpr' = simple_arr pexpr' allbindings in
						(* begin dumping array as pexpr *)
						let _ = Hashtbl.replace ufs pexpr () in
						let path = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr)) in
						let returnvalue = (fn ^ "_r", (try Hashtbl.find assignment path with _ -> assert false)) in
						let paramvalues = Misc.mapi (fun e i -> match e with
							| Var e' -> (fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
							| PInt e' -> (fn ^ "_" ^ (string_of_int i), e')
							| Binop _ -> 
								let e' = Path.Pident (Ident.create_persistent (pexpr_to_string e)) in
								(fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
							| _ -> (** Fixme? *) assert false
							) es in
						let value = paramvalues @ [returnvalue] in
						(* begin dumping the pair of the above *)
						let _ = Hashtbl.replace ufs pexpr' () in
						let path' = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr')) in
						let value' = (fn ^ "_r'", (try Hashtbl.find assignment path' with _ -> assert false)) in
						let paired_value = value @ [value'] in
						if (Hashtbl.mem hos fn) then (Hashtbl.replace hos fn (paired_value::(Hashtbl.find hos fn)); pexpr)
						else (Hashtbl.replace hos fn [paired_value]; pexpr)
					(* end dumping paired array loggings *)
					else (* dump single array logging and function logging *)
					let _ = Hashtbl.replace ufs pexpr () in
					let path = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr)) in
					let returnvalue = (fn ^ "_r", (try Hashtbl.find assignment path with _ -> assert false)) in
					let paramvalues = Misc.mapi (fun e i -> match e with
						| Var e' -> (fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
						| PInt e' -> (fn ^ "_" ^ (string_of_int i), e')
						| Binop _ -> 
							let e' = Path.Pident (Ident.create_persistent (pexpr_to_string e)) in
							(fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
						| _ -> (** Fixme? *) assert false
						) es in
					let value = paramvalues @ [returnvalue] in
					(*(store := (value @ (!store)); pexpr)*)
					if (Hashtbl.mem hos fn) then (Hashtbl.replace hos fn (value::(Hashtbl.find hos fn)); pexpr)
					else (Hashtbl.replace hos fn [value]; pexpr)
				| _ -> pexpr
				) bad_constraint);
			(*Common.remove_duplicates (!store)*)
			(** Flatten what we have recorded in plains and hos *)
			let store = (Hashtbl.fold (fun k v res -> v::res) plains []) @ (Hashtbl.fold (fun k v res -> v::res) hos []) in
			List.map (List.flatten) (Misc.lflap store))
			) assignments) ) in
			
			let bad_assignments = transl_assignments bad_assignments bad_constraint in
			let bad_assignments = 
				if (List.length bad_assignments > 1) then
					check_neg_sample bad_constraint bad_assignments
				else bad_assignments in
			(*let bad_assignments = check_neg_sample bad_constraint bad_assignments in*)
			bad_assignments in
	
	Hashtbl.iter (fun path (prerestrictions, postrestrictions, {pre=prebad; post=postbad}) ->
		let prebad_assignments = 
				List.fold_left (fun res restriction -> 
					res @ (generate path restriction prebad postbad)
				) [] prerestrictions in
		let postbad_assignments = 
				List.fold_left (fun res restriction -> 
					res @ (generate path restriction postbad postbad)
				) [] (postrestrictions) in
		(** Prepare a var for function application *)
		(*let funfr = Hashtbl.find funframebindings path in
		let allbindings = Frame.get_fun_bindings env funfr in
		let unsounds = Hashtbl.find unsounds path in
		let _ = List.iter (fun u -> Format.fprintf Format.std_formatter "Unsound %a@." Predicate.pprint_pexpr u) unsounds in
		(* We want to care about for-all array property *)
		let arr_pair = Backwalker.detect_arr_adj_pattern postbad allbindings unsounds in
		let _ = Format.fprintf Format.std_formatter "ARRPAIR:@." in
		let _ = List.iter (fun (p, p') -> Format.fprintf Format.std_formatter "(%a, %a)@." 
						Predicate.pprint_pexpr p Predicate.pprint_pexpr p') arr_pair in
		let pre_bad_constraint = simplify_funapp prebad allbindings in
		let post_bad_constraint = simplify_funapp postbad allbindings in
		let prebadpred = big_and (pre_bad_constraint :: (getFunApp pre_bad_constraint)) in 
		let postbadpred = big_and (post_bad_constraint :: (getFunApp post_bad_constraint)) in 
		let prebad_assignments = iter_solve prebadpred neg_number in
		let postbad_assignments = iter_solve postbadpred neg_number in
		
		let transl_assignments assignments bad_constraint = 
			(List.flatten (List.map (fun assignment -> 
			(*let store = ref [] in*)
			let plains = Hashtbl.create 5 in
			let hos = Hashtbl.create 5 in
			(* We want to deal with each uninterpreted function only once *)
			let ufs = Hashtbl.create 5 in
			(ignore (Predicate.map_expr (fun pexpr -> match pexpr with
				| Predicate.Var path -> 
					let value = (try Hashtbl.find assignment path with _ -> assert false) in
					(*(store := (Path.name path, value) :: (!store); pexpr)*)
					(Hashtbl.replace plains (Path.name path) [[(Path.name path, value)]]; pexpr)
				| Predicate.FunApp (fn, es) -> 
					if (Hashtbl.mem ufs pexpr) then pexpr else (* unsound application is filtered out *)
					if (List.exists (fun u -> pexpr = u) unsounds) then pexpr else
					(* if this funapp appears in array adj dection, specially encode it *)
					if (List.exists (fun (_, pe) -> (simple_arr pe allbindings) = pexpr) arr_pair) then pexpr else
					if (List.exists (fun (pe, _) -> (simple_arr pe allbindings) = pexpr) arr_pair) then
					(* dumping paired array loggings *)
						let (_, pexpr') = List.find (fun (pe, _) -> (simple_arr pe allbindings) = pexpr) arr_pair in
						let pexpr' = simple_arr pexpr' allbindings in
						(* begin dumping array as pexpr *)
						let _ = Hashtbl.replace ufs pexpr () in
						let path = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr)) in
						let returnvalue = (fn ^ "_r", (try Hashtbl.find assignment path with _ -> assert false)) in
						let paramvalues = Misc.mapi (fun e i -> match e with
							| Var e' -> (fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
							| PInt e' -> (fn ^ "_" ^ (string_of_int i), e')
							| Binop _ -> 
								let e' = Path.Pident (Ident.create_persistent (pexpr_to_string e)) in
								(fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
							| _ -> (** Fixme? *) assert false
							) es in
						let value = paramvalues @ [returnvalue] in
						(* begin dumping the pair of the above *)
						let _ = Hashtbl.replace ufs pexpr' () in
						let path' = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr')) in
						let value' = (fn ^ "_r'", (try Hashtbl.find assignment path' with _ -> assert false)) in
						let paired_value = value @ [value'] in
						if (Hashtbl.mem hos fn) then (Hashtbl.replace hos fn (paired_value::(Hashtbl.find hos fn)); pexpr)
						else (Hashtbl.replace hos fn [paired_value]; pexpr)
					(* end dumping paired array loggings *)
					else (* dump single array logging and function logging *)
					let _ = Hashtbl.replace ufs pexpr () in
					let path = Path.Pident (Ident.create_persistent (pexpr_to_string pexpr)) in
					let returnvalue = (fn ^ "_r", (try Hashtbl.find assignment path with _ -> assert false)) in
					let paramvalues = Misc.mapi (fun e i -> match e with
						| Var e' -> (fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
						| PInt e' -> (fn ^ "_" ^ (string_of_int i), e')
						| Binop _ -> 
							let e' = Path.Pident (Ident.create_persistent (pexpr_to_string e)) in
							(fn ^ "_" ^ (string_of_int i), (try Hashtbl.find assignment e' with _ -> assert false))
						| _ -> (** Fixme? *) assert false
						) es in
					let value = paramvalues @ [returnvalue] in
					(*(store := (value @ (!store)); pexpr)*)
					if (Hashtbl.mem hos fn) then (Hashtbl.replace hos fn (value::(Hashtbl.find hos fn)); pexpr)
					else (Hashtbl.replace hos fn [value]; pexpr)
				| _ -> pexpr
				) bad_constraint);
			(*Common.remove_duplicates (!store)*)
			(** Flatten what we have recorded in plains and hos *)
			let store = (Hashtbl.fold (fun k v res -> v::res) plains []) @ (Hashtbl.fold (fun k v res -> v::res) hos []) in
			List.map (List.flatten) (Misc.lflap store))
			) assignments) ) in
		
		let prebad_assignments = transl_assignments prebad_assignments pre_bad_constraint in
		let postbad_assignments = transl_assignments postbad_assignments post_bad_constraint in*)
		Hashtbl.replace samples path {spre = prebad_assignments; spost = postbad_assignments}
		) badbindings