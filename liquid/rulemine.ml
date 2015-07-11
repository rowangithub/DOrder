open Asttypes
open Typedtree
open Btype
open Types
open Frame
open Unix

exception MINEFAIL

(* A way to represent program positive samples *)
type value = Cell of int | Cells of (string * value) list list
type sample = (string, value) Hashtbl.t

(***********************************************)
(*************** Util Funcitons ****************)
(***********************************************)
let remove_nth l indices = 
	fst (List.fold_left (fun (l',index) li -> 
		if (List.exists (fun i -> i = index) indices) 
		then (l', index+1)
		else (l'@[li], index+1)	
	) ([],0) l)


let return_repr = "r"

let dump_samples samples =
	let outch = open_out ("./rulesamples.tab") in
	(List.iter (fun sample -> 
		let len = List.length sample in
		let i = ref 0 in
		List.iter (fun v ->
			if (!i < len-1) then 
				Printf.fprintf outch "%d " v	
			else 
				Printf.fprintf outch "%d\n" v;
			(i := !i+1)
		) sample
	) samples; close_out outch)

(* 
both
x head (consequence)
y body (antecedence) *)
let dump_formats precedences consequences = 
	let outch = open_out ("./ruleapp") in
	(Printf.fprintf outch "%s\n" "both";
	List.iter (fun conseq -> Printf.fprintf outch "%s head\n" (string_of_int conseq)) consequences;
	List.iter (fun preced -> Printf.fprintf outch "%s body\n" (string_of_int preced)) precedences;
	close_out outch)

let mine () = 
	let code = fork () in
  match code with
    | 0 -> (
			try execvp "./apriori" 
					[|"./apriori"; "-tr"; "-s31"; "-c100"; "-v %X %C"; "./rulesamples.tab"; "./rule"; "./ruleapp"|]
      with _ -> 
				Format.fprintf Format.err_formatter "@[%s@]@." "error while execv Apriori\n"; raise MINEFAIL)
    | -1 -> 
			(Format.fprintf Format.err_formatter "@[%s@]@." "error accured on Apriori fork\n"; raise MINEFAIL)
    | _ -> (
			let (_, status) = wait () in 
			match status with
				| WEXITED code -> 
					if (code = 0) then Format.fprintf Format.err_formatter "@[%s@]@." "Apriori mined...\n"
					else (assert false)
				| _ -> assert false)

let read_invariant predicates = 
	let lines = ref [] in
	let chan = open_in "./rule" in
	(try
  	while true; do
    	lines := input_line chan :: !lines
  	done; []
	with End_of_file ->
  	close_in chan;
  let lines = List.rev !lines in
	List.map (fun line -> 
		let stats = Str.split (Str.regexp "[ \t]+") line in
		let len = List.length stats in
		let (precedence, consequence, support, confidence, _) = 
		List.fold_left (fun (precedence, consequence, support, confidence, i) stat -> 
			if (i = 0) then (precedence, consequence @ [stat], support, confidence, i+1)
			else if (i = 1) then 
				(assert (String.compare "<-" stat = 0); 
				(precedence, consequence, support, confidence, i+1))
			else if (i = len-2) then (* support *)
				(precedence, consequence, float_of_string stat, confidence, i+1)
			else if (i = len-1) then (* confidence *)
				(precedence, consequence, support, int_of_string stat, i+1)
			else (* Must be for precedences *)
				(precedence @ [stat], consequence, support, confidence, i+1)
			) ([], [], 0.0, 0, 0) stats in
		let _ = assert (support > 0.0 && confidence > 0) in
		let precedence = Predicate.big_and (List.map (fun stat -> 
			let index = int_of_string stat in
			if (index mod 2 = 0) then List.nth predicates (index/2) 
			else Predicate.Not (List.nth predicates ((index-1)/2))
			) precedence) in
		let consequence = Predicate.big_and (List.map (fun stat -> 
			let index = int_of_string stat in
			if (index mod 2 = 0) then List.nth predicates (index/2) 
			else Predicate.Not (List.nth predicates ((index-1)/2))
			) consequence) in
		((Predicate.implies (precedence, consequence)), support, confidence)
	) lines)

(** learning association rules for arrays and containers 
    Interesting properties are mined from postive samples only 
		Each function correpsonds to a pair p
		fst(p) = the set of predicate based samples 
		snd(p) = the set of array information ainfo 
			fst(ainfo) = array name
			snd(ainfo) = predicates must appear in antecedents 
			trd(ainfo) = predicates must appear in consequences 
		Intuition: antecedents are predicates constraining array indices 
		while consequences are predicates constraints array sub results *)
let naive_mine_assoc (samples: (int list list)) predicates env fr =
	(** Firstly remove all common attributes *)
	let commons = ref [] in
	let _ = 
		for iter = 0 to ((List.length predicates)-1) do
			match (List.fold_left (fun res sample -> match res with
				| Some v ->
					let v' = List.nth sample iter in
					if (v = v') then Some v else None
				| None -> None 	
			) (Some (List.nth (List.hd samples) iter)) (samples)) with
				| Some v -> 
					if (v = 1) then (commons:=(iter)::(!commons)) 
					else (commons:=(iter)::(!commons))
				| None -> ()
			done;
	in
	let common_indices = (!commons) in
	let predicates = remove_nth predicates common_indices in
	let samples = List.map (fun sample -> remove_nth sample common_indices) samples in
	(** Fixme. We only care about the last array? *)
	let allbindings = Frame.get_fun_bindings env fr in
	let arrs = List.fold_left (fun res (p, f) -> match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
			(** check whether adjacent elements of p is constrainted in badf *)
			res @ [p]
		|	_ -> res
	) [] allbindings in
	let arr = List.nth arrs ((List.length arrs)-1) in
	(** Secondly remove all non-array elements *)
	let (nonarray,_) = List.fold_left (fun (res,i) predicate -> 
		let vars = Predicate.vars predicate in
		let cands = Array.init 5 (fun i -> (Path.name arr) ^ "_" ^ (string_of_int i)) in
		let cands = ((Path.name arr) ^ "_" ^ return_repr)::(Array.to_list cands) in
		if (List.exists (fun var -> 
			(String.compare (Path.name var) ((Path.name arr) ^ "_" ^ return_repr) != 0) &&
			(Common.str_contains (Path.name var) ((Path.name arr) ^ "_" ^ return_repr))
			) vars) then (res @ [i], i+1)
		else if (List.exists (fun var -> List.exists (fun cand -> 
			String.compare cand (Path.name var) = 0
			) cands) vars) then (res, i+1)
		else (res @ [i], i+1)
	) ([],0) predicates in
	let predicates = remove_nth predicates nonarray in
	let samples = List.map (fun sample -> remove_nth sample nonarray) samples in
	(** Begin rule mining *)
	(*0. Make sample complete *)
	let samples = List.map (List.fold_left (fun res v -> 
		(res @ [v] @ [1-v])
	) []) samples in
	(*1. Find the attributes that must appear in precedence and confidence *)
	let (precedences, consequences, _) = 
	List.fold_left (fun (preced, conseq, i) predicate -> 
		let vars = Predicate.vars predicate in
		if (List.exists (fun var -> 
			String.compare (Path.name var) ((Path.name arr) ^ "_" ^ return_repr) = 0
			) vars) then (preced, conseq @ [2*i] @ [2*i+1], i+1) 
		else if (List.exists (fun var -> 
			(** Fixme? *)
			let cands = Array.init 5 (fun i -> (Path.name arr) ^ "_" ^ (string_of_int i)) in
			List.exists (fun cand -> String.compare cand (Path.name var) = 0) (Array.to_list cands)
			) vars) then (preced @ [2*i] @ [2*i+1], conseq, i+1)
		else assert false	
	) ([], [], 0) predicates in
	(*2. Create the input file for Apriori Algorithm *)
	let samples = List.map (fun sample -> fst (List.fold_left (fun (res,i) v -> 
		if (v = 1) then (res@[i], i+1) else (res, i+1)
	) ([],0) sample)) samples in
	(*4. Invoke Apriori Algorithm *)
	let _ = (dump_samples samples; 
	dump_formats precedences consequences; mine ()) in
	(*4. Read and return results from Apriori Algorithm *)
	read_invariant predicates
	
(******* Util Function *******)
let find_index items it = 
	let (res, _) = List.fold_left (fun (res, i) item -> match res with
		| Some res -> (Some res, i+1)
		| None ->  (
				if (item = it) then (Some i, i+1)
				else (None, i+1) 
			)
	) (None,0) items in
	match res with
		| Some res -> res
		| None -> (Format.fprintf Format.std_formatter "it unfound%a@." Predicate.pprint it; assert false)
		
(** learning rules manually *)
let manual_mine_assoc (samples: (int list list)) predicates env fr tbl =
	let find_commons predicates samples = 
		let commons = ref [] in
		let _ = 
			for iter = 0 to ((List.length predicates)-1) do
				match (List.fold_left (fun res sample -> match res with
					| Some v ->
						let v' = List.nth sample iter in
						if (v = v') then Some v else None
					| None -> None 	
				) (Some (List.nth (List.hd samples) iter)) (samples)) with
					| Some v -> 
						if (v = 1) then (commons:=(iter,(List.nth predicates iter))::(!commons)) 
						else (commons:=(iter,(Predicate.Not (List.nth predicates iter)))::(!commons))
					| None -> ()
				done;
		in List.split (!commons) in
	(*let remove_indices predicates samples indices = 
		let predicates = remove_nth predicates indices in
		let samples = List.map (fun sample -> remove_nth sample indices) samples in
		(predicates, samples) in*)
	let find_samples predicates samples intrst_preds = 
		List.filter (fun sample -> 
			let indices = List.map (fun intrst_pred -> match intrst_pred with
				| Predicate.Atom (v1, _, Predicate.PInt _) ->
					(find_index predicates intrst_pred, 1) 
				| Predicate.Atom (v1, Predicate.Ge, v2) -> 
						let index = (* (v2 > v1) *)
							find_index predicates (Predicate.Atom (v2, Predicate.Gt, v1)) in
						(index, 0)
				| Predicate.Atom (v1, Predicate.Gt, v2) -> 
						let index = (* (v1 > v2) *)
							find_index predicates (Predicate.Atom (v1, Predicate.Gt, v2)) in
						(index, 1)
				| Predicate.Atom (v1, Predicate.Le, v2) ->
						let index = (* (v1 > v2) *)
							find_index predicates (Predicate.Atom (v1, Predicate.Gt, v2)) in
						(index, 0)
				| Predicate.Atom (v1, Predicate.Lt, v2) -> 
						let index = (* (v2 > v1) *)
							find_index predicates (Predicate.Atom (v2, Predicate.Gt, v1)) in
						(index, 1)
				| _ -> (Format.fprintf Format.std_formatter "Error predicate %a@." Predicate.pprint intrst_pred; assert false)	
			) intrst_preds in
			List.for_all (fun (index, tv) -> (List.nth sample index = tv)) indices	
		) samples in
	
	(** Fixme. We only care about the last array? *)
	let allbindings = Frame.get_fun_bindings env fr in
	let arrs = List.fold_left (fun res (p, f) -> match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
			(** check whether adjacent elements of p is constrainted in badf *)
			res @ [p]
		|	_ -> res
	) [] allbindings in
	let arr = List.nth arrs ((List.length arrs)-1) in
	(** Secondly remove all non-array elements *)
	(*let (nonarray,_) = List.fold_left (fun (res,i) predicate -> 
		let vars = Predicate.vars predicate in
		let arrays = [((Path.name arr) ^ "_" ^ return_repr)] in
		if (List.exists (fun var -> 
			(String.compare (Path.name var) ((Path.name arr) ^ "_" ^ return_repr) != 0) &&
			(Common.str_contains (Path.name var) ((Path.name arr) ^ "_" ^ return_repr))
			) vars) then (res @ [i], i+1)
		else if (List.exists (fun var -> List.exists (fun cand -> 
			String.compare cand (Path.name var) = 0
			) arrays) vars) then (res, i+1)
		else (res @ [i], i+1)
	) ([],0) predicates in*)
	(*1. Seek all common predicates that do not mention array *)
	let (commonpreds_indices, commonpreds) = find_commons predicates samples in
	(*let (predicates, samples) = remove_indices predicates samples commonpreds_indices in*)
	let commonpreds = List.filter (fun commonpred -> 
		let vars = Predicate.vars commonpred in
		List.for_all (fun var -> List.for_all (fun arr ->
			not (Common.str_contains (Path.name var) ((Path.name arr) ^ "_"))
			) arrs) vars	
	) commonpreds in
	(*2. For each array dimension, enumerate all above predicates 
				seek all common predicates that do mentaion array *)
	let d = 0 in
	let rules = ref [] in
	let a_d = Predicate.Var (Hashtbl.find tbl ((Path.name arr) ^ "_" ^ (string_of_int d))) in
	(List.iter (fun commonpred -> 
		(*3. Generate rule antecedents *)
		let _ = Format.fprintf Format.std_formatter "rm:common pred = %a@." Predicate.pprint commonpred in
		let rule_antecedents = match commonpred with
			| Predicate.Atom (v1, Predicate.Gt, v2) 
			| Predicate.Atom (v1, Predicate.Ge, v2)
			| Predicate.Not (Predicate.Atom (v2, Predicate.Gt, v1)) -> 
				[[Predicate.Atom (v1,Predicate.Ge, a_d); Predicate.Atom (a_d,Predicate.Ge,v2)];
				[Predicate.Atom (v1,Predicate.Ge, a_d); Predicate.Atom (a_d,Predicate.Gt,v2)];
				[Predicate.Atom (v1,Predicate.Gt, a_d); Predicate.Atom (a_d,Predicate.Ge,v2)];
				[Predicate.Atom (v1,Predicate.Gt, a_d); Predicate.Atom (a_d,Predicate.Gt,v2)]]
			(*| Predicate.Atom (v1, Predicate.Le, v2) 
			| Predicate.Atom (v1, Predicate.Lt, v2) -> 
				[[Predicate.Atom (v1,Predicate.Le,a_d); Predicate.Atom (a_d,Predicate.Le,v2)];
				[Predicate.Atom (v1,Predicate.Le,a_d); Predicate.Atom (a_d,Predicate.Lt,v2)];
				[Predicate.Atom (v1,Predicate.Lt,a_d); Predicate.Atom (a_d,Predicate.Le,v2)];
				[Predicate.Atom (v1,Predicate.Lt,a_d); Predicate.Atom (a_d,Predicate.Lt,v2)]]*)
			| _ -> assert false
			in
		List.iter (fun antecedent -> 
			let _ = List.iter (fun a -> 
				Format.fprintf Format.std_formatter "antecedent: %a@." Predicate.pprint a) antecedent in
			let samples = try find_samples predicates samples antecedent with _ -> [] in 
			let (commonpreds_indices, rule_consequences) = try find_commons predicates samples with _ -> ([],[]) in
			(*let rule_consequences = List.filter (fun conseq -> 
				let vars = Predicate.vars conseq in
				let 	
			) rule_consequences in*)
			match rule_consequences with
				| [] -> ()
				| (_ as consequence) -> 
					(rules:=(Predicate.implies (Predicate.big_and antecedent, Predicate.big_and consequence))::(!rules))
		) rule_antecedents			
		(*match (List.fold_left (fun res antecedent -> match res with
			| Some res -> Some res
			| None -> 
				let _ = List.iter (fun a -> 
					Format.fprintf Format.std_formatter "antecedent: %a@." Predicate.pprint a) antecedent in
				let samples = find_samples predicates samples antecedent in 
				let (commonpreds_indices, rule_consequences) = find_commons predicates samples in
				match rule_consequences with
					| [] -> None
					| (_ as consequence) -> Some (Predicate.implies (Predicate.big_and antecedent, Predicate.big_and consequence))
		) None rule_antecedents	) with
			| Some res -> (rules := res::(!rules))
			| None -> ()*)
	) commonpreds;
	(!rules))
	
(***************************************************)
(* Normalize the invariants for decidable checking *)
(***************************************************)		
(* search a special variable and split the expression into two part
		one is the special variable and the other one is the rest *)
let search_exp fname expr = 
	let rec loop expr = match expr with
		| Predicate.Var maybevar -> 
			if (String.compare (Path.name maybevar) fname = 0) 
			then (true, expr, None) else (false, expr, None)
  	| Predicate.Binop (e1, op, e2) -> 
			let (flag1, e1, sign1) = loop e1 in
			let (flag2, e2, sign2) = loop e2 in
			(match (flag1, flag2) with
				| (false, true) when op = Predicate.Plus -> (false, e1, Some Predicate.Plus)
				| (false, true) when op = Predicate.Minus -> (false, e1, Some Predicate.Minus)
				| _ -> (false, expr, match (sign1, sign2) with 
					| (Some _, Some _) -> assert false
					| (Some _, None) -> sign1
					| (None, Some _) -> sign2
					| _ -> None))
  	| _ -> (false, expr, None) in
	let (_, restexpr, sign) = loop expr in
	match sign with
		| Some sign when sign = Predicate.Plus -> 
			let var = List.find (fun exp_var -> 
				(String.compare (Path.name exp_var) fname) = 0) (Predicate.exp_vars expr) in
			(var, Predicate.Binop (Predicate.Var var, Predicate.Minus,restexpr))
		| Some sign when sign = Predicate.Minus -> 
			let var = List.find (fun exp_var -> 
				(String.compare (Path.name exp_var) fname) = 0) (Predicate.exp_vars expr) in
			(var, Predicate.Binop (restexpr, Predicate.Minus, Predicate.Var var))
		| _ -> assert false

(* Remove constants for template constraints *)
let ignore_constant pred = Predicate.map_expr (fun expr -> match expr with
	| Predicate.Binop (e1, Predicate.Plus, e2) -> (match (e1, e2) with
		| (Predicate.Var p, _) when (String.compare (Path.name p) "d" = 0) -> e2
		| (_, Predicate.Var p) when (String.compare (Path.name p) "d" = 0) -> e1
		| _ -> expr)
	| _ -> expr
) pred

(** Normalized all the exisitentials in pred *)
let normalize_existentials pred = 
	let fs = Predicate.get_all_funs pred in
	let bases = List.fold_left (fun res f -> 
		match f with
		| Predicate.FunApp (c, args) -> 
			if (List.length args = 1) then 
				let arg = List.hd args in
				match arg with
					| Predicate.Var arg when (String.compare ("ex_"^c^"_i") (Path.name arg) = 0) -> res
					| _ -> 
						if (List.exists (fun ev -> (String.compare ("ex_"^c^"_i") (Path.name ev) = 0)) 
								(Predicate.exp_vars arg) )
						then (res@[(c,arg)]) else res
			else res
		| _ -> assert false) [] fs in
	let pred = List.fold_left (fun pred (fname, base) -> 
		(try let temp_var = Path.mk_ident "temp" in
		let (var, substract_var_base) = search_exp ("ex_"^fname^"_i") base in
		let pred = Predicate.map_expr (fun expr ->
			if (expr = Predicate.FunApp (fname, [base])) 
			then Predicate.FunApp (fname, [Predicate.Var temp_var]) else expr) pred in
		let pred = Predicate.subst substract_var_base var pred in
		(Predicate.subst (Predicate.Var var) temp_var pred) with _ -> assert false)	
	) pred bases in
	let _ = Format.fprintf Format.std_formatter "Normalized to %a@." Predicate.pprint pred in
	pred
						
(** Simplify the generated predicate from learning/mining *)	
let normalize pred fp = 
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
		)	pred in
	(*let _ = Format.fprintf Format.std_formatter "beginpred = %a@." Predicate.pprint pred in*)
	let pred = simply_coeff pred in
	(*let _ = Format.fprintf Format.std_formatter "endpred = %a@." Predicate.pprint pred in*)
	let fs = Predicate.get_all_funs pred in
	let bases = Hashtbl.create 3 in
	let _ = List.iter (fun f -> match f with
		| Predicate.FunApp (c, args) ->
			(if (List.length args = 1) then
				let arg = List.hd args in
				match arg with
					| Predicate.Var arg when (String.compare (c^"_i") (Path.name arg) = 0 ||
							String.compare (c^"_i") (Frame.old_array_flag^(Path.name arg)) = 0) -> 
								Hashtbl.replace bases (c) (false, [])
					| _ -> 
						if (List.exists (fun ev -> (String.compare (c^"_i") (Path.name ev)) = 0 ||
							(String.compare (c^"_i") (Frame.old_array_flag^(Path.name ev))) = 0) (Predicate.exp_vars arg)) then 
								if (Hashtbl.mem bases c) then
									let (flag, res) = Hashtbl.find bases c in
									if (flag) then Hashtbl.replace bases c (flag, res@[arg]) else ()
								else Hashtbl.replace bases c (true, [arg])
						else ()
				else ())
		| _ -> assert false
	) fs in
	let _ = assert (Hashtbl.length bases = 1) in (** Just for our puropose *)
	let pred = 
		Hashtbl.fold (fun fp (_, bases) res -> 
			List.fold_left (fun res base -> 
				(try let temp_var = Path.mk_ident "temp" in
				let c = 
					if (Common.str_contains fp Frame.old_array_flag) then 
						String.sub fp (String.length Frame.old_array_flag) (String.length fp - String.length Frame.old_array_flag)
					else fp in
				let (var, substract_var_base) = search_exp (c^"_i") base in
				let pred = Predicate.map_expr (fun expr ->
					if (expr = Predicate.FunApp (fp, [base])) 
					then Predicate.FunApp (fp, [Predicate.Var temp_var]) else expr) pred in
				let pred = Predicate.subst substract_var_base var pred in
				(Predicate.subst (Predicate.Var var) temp_var pred) with _ -> assert false)
			) res bases 
		) bases pred in
	(*let _ = Format.fprintf Format.std_formatter "This is normalized to %a@." Predicate.pprint pred in*)
	normalize_existentials pred	

(**************************************************)
(* Automatically mine invaiants from data samples *)
(**************************************************)
(* Translate logged sample into cells 
		type value = Cell of int | Cells of (string * value) list list
		type sample = (string, value) Hashtbl.t	*)	
let transel_sample ispost goods names arrs (nametbl:(string, Path.t) Hashtbl.t) = 
	(** Notepad: put oldarray into tbl so they can appear in the sample *)
	List.map (fun (locals, envs) ->
		(** To infer how array values are preserved in the postcondition *)
		let _ = if (ispost) then
			List.iter (fun arr -> 
				Hashtbl.replace nametbl (Frame.old_array_flag ^ (Path.name arr)) 
					(Path.Pident (Ident.create_persistent (Frame.old_array_flag ^ (Path.name arr))))
			) arrs in	 
		let tbl = Hashtbl.create 9 in
		let _ = List.iter (fun local -> 
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			(*let _ = Format.fprintf Format.std_formatter "PAIRSLOG:%s:%s " name_name name_value in*)
			if (List.exists (fun n -> (String.compare n name_name = 0)) names) then 
				if (Hashtbl.mem tbl name_name) then
					Hashtbl.replace tbl name_name ((Hashtbl.find tbl name_name) @ [name_value])
				else Hashtbl.replace tbl name_name [name_value]
			) locals in	
		(*let _ = Format.fprintf Format.std_formatter "@." in*)
		(* Match what is logged in one callsite to a new sample and tbl : (string, string list) Hashtbl.t *)
		let sample = Hashtbl.create (List.length locals) in
		(Hashtbl.iter (fun n vs -> 
			if (List.length vs = 0) then assert false
			else if (List.length vs = 1) then
				let v = List.hd vs in
				let n = Hashtbl.find nametbl n in
				Hashtbl.replace sample n (Cell (int_of_string v))
			else 
				(* We should remove the Instrument.tmp_prefix and Instrument.tmp_postfix  *)
				let n = 
					if (Common.str_contains n Instrument.tempt_arr_prefix) then
						let n = String.sub n (String.length Instrument.tempt_arr_prefix)
								(String.length n - String.length Instrument.tempt_arr_prefix) in
						if (ispost) then (Frame.old_array_flag ^ n) (** 1. advance for oldarray case *)
						else n
					else if (Common.str_contains n Instrument.tempt_arr_postfix) then
						String.sub n (String.length Instrument.tempt_arr_postfix) 
							(String.length n - String.length Instrument.tempt_arr_postfix)
					else n in
				let pn = Hashtbl.find nametbl n in
				let n = (** 2. roll back for oldarray case *)
					if (Common.str_contains n Frame.old_array_flag) then 
						String.sub n (String.length Frame.old_array_flag) 
							(String.length n - String.length Frame.old_array_flag)
					else n in
				Hashtbl.replace sample pn (Cells (
					List.map (fun v -> 
						if (String.contains v '#') then
							let sub_locals = Str.split (Str.regexp ",") v in
							let sub_locals = List.map (fun sub_local -> 
								let sub_name = Str.split (Str.regexp "#") sub_local in
								(List.hd sub_name, Cell (int_of_string (List.nth sub_name 1)))
								) sub_locals in
							if (List.exists (fun (k, v) -> (String.compare k (n^"_r") = 0)
								) sub_locals) then (* complete record *) sub_locals
							else (* missing value. remove this record *) 
								(Format.fprintf Format.std_formatter "n = %s@." n; assert false)
						else assert false	
					) vs))
		) tbl; sample)) goods

(** To infer how array values are preserved from iterations to iterations *)
(* old arrays are irrelevant if we are not intersted in preservation property *)	
let filter_old_array samples = 
	List.map (fun sample -> 
		let oldarraykeys = [] in
		(List.iter (fun key -> Hashtbl.remove sample key) oldarraykeys;
		sample)
	) samples		
		
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
		if (List.length solutions > 16) then (solutions@[new_solution])
		else allsolution (solutions@[new_solution]) constraint_f cvars
		
(** Collect the global array information. 
	Log whether the array is read-only and its index accesses *)
let collect_arr_info effcons pred arrs arr_info = 
	let wrotearrays = ref [] in
	let _ = ignore (Predicate.map_pred (fun pred -> match pred with
		| Predicate.Atom (_, Predicate.Eq, Predicate.FunApp (fn, args)) 
			when (String.compare fn "UF" = 0) -> 
			let aname = List.hd args in
			(match aname with
				| Predicate.Var apath when (List.exists (fun arr -> Path.same arr apath) arrs) ->
					(wrotearrays := apath::(!wrotearrays); pred)
				| _ -> pred)
		| _ -> pred) effcons) in
	let wrotearrays = Common.remove_duplicates (!wrotearrays) in
	ignore (Predicate.map_expr (fun expr -> match expr with
		| Predicate.FunApp (f, args) when (String.compare f "UF" = 0) ->
			let aname = List.hd args in
			let aindices = List.tl args in
			((match aname with
				| Predicate.Var apath when (List.exists (fun arr -> Path.same arr apath) arrs) ->
					let indices = 
						List.fold_left (fun res aindex -> res @ (Predicate.exp_vars aindex)) [] aindices in
					if (Hashtbl.mem arr_info apath) then
						Hashtbl.replace arr_info apath (
							let (readonly, aindices) = Hashtbl.find arr_info apath in
							(readonly, Common.remove_duplicates (aindices @ indices)))
					else
						let readonly = not (List.exists (fun wrotearray -> 
							(Path.same wrotearray apath)) (wrotearrays)) in 
						Hashtbl.replace arr_info apath (readonly, Common.remove_duplicates indices)
				| _ -> ()); expr)
		| _ -> expr
	) (Predicate.And (effcons, pred)))

(** It is best to generate template from underapproximate symbolic precondition *)	
let preprocess_template arrs pred = 
	let arr_indices = Hashtbl.create 5 in
	let arr_cmps = Hashtbl.create 5 in
	let rec accumulate pexp = match pexp with 
		| Predicate.FunApp (f, args) when (String.compare f "UF" = 0) -> 
			let aname = List.hd args in
			let aindices = List.tl args in
			(match aname with 
				| Predicate.Var apath when (List.exists (fun arr -> Path.same arr apath) arrs) ->
					let indices = 
						List.fold_left (fun res aindex -> res @ (Predicate.exp_vars aindex)) [] aindices in
					if (Hashtbl.mem arr_indices apath) then
						Hashtbl.replace arr_indices apath (Common.remove_duplicates
							((Hashtbl.find arr_indices apath) @ indices))
					else Hashtbl.replace arr_indices apath (Common.remove_duplicates indices) 
				| _ -> List.iter (fun arg -> accumulate arg) args)
		| Predicate.FunApp (f, args) -> List.iter (fun arg -> (accumulate arg)) args 
		| Predicate.Binop (p1, _, p2) -> (accumulate p1; accumulate p2)
		| Predicate.Field (_, p) -> accumulate p
		| Predicate.Proj (_, p) -> accumulate p
		| Predicate.Var p -> Hashtbl.replace arr_cmps p ()
		| Predicate.PInt _ -> () in
	match pred with
		| Predicate.Atom (p1, rel, p2) -> 
			(accumulate p1; accumulate p2;
			(* The variables in arr_indices should appear in array domain part only
			   The variables in arr_cmps should not appear in array domain part 
				 But, for generality, we only require variable either not in arr_cmps or
				 in arr_indices to appear in array domain *)
			(arr_indices, arr_cmps)
			)
		| _ -> assert false	

(** Prepare for plains *)
let generate_arith_plains_template_wo_d plains coeffs ds = 
	let (t,_,effs) = List.fold_left (fun (res,i,effs) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi () in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1,effs@[xi])
	) (Predicate.PInt 0, 0, []) plains in
	(t,effs)

let generate_arith_plains_template plains coeffs ds = 
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	(*let d = Predicate.PInt 0 in*)
	let (t,_,effs) = List.fold_left (fun (res,i,effs) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi () in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1,effs@[xi])
	) (d, 0, []) plains in
	(t,effs)

(* Here the length of array a is counted *)	
let generate_arith_plains_template_w_len_wo_d plains array coeffs ds = 
	let (t,i,effs) = List.fold_left (fun (res,i,effs) plain ->
		let xi = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
		let _ = Hashtbl.replace coeffs xi () in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1,effs@[xi])
	) (Predicate.PInt 0, 0, []) plains in
	let xn = Predicate.Var (Path.mk_ident ("x_"^(string_of_int i))) in
	let _ = Hashtbl.replace coeffs xn () in
	let t = Predicate.Binop (t, Predicate.Plus, 
			Predicate.Binop (xn, Predicate.Times, Predicate.Var (
				Path.Pident (Ident.create_with_stamp ("len_"^(Path.name array)) (Path.stamp array))))) in
	(t,effs@[xn]) 	

(* It is for arrays with exactly the same index in terms of excluding constants) *)	
let generate_shared_arith_plains_template plains coeffs ds = 
	let d = Predicate.Var (Path.mk_ident "d") in
	let _ = Hashtbl.replace ds d () in 
	let _ = List.iter (fun plain -> Format.fprintf Format.std_formatter "plain=%s@." (Path.name plain)) plains in
	let _ = List.iter (fun coeff -> Format.fprintf Format.std_formatter "coeff=%a@." Predicate.pprint_pexpr coeff) coeffs in
	let _ = assert (List.length plains = List.length coeffs) in
	let (t,_) = List.fold_left (fun (res,i) plain ->
		let xi = List.nth coeffs i in
		(Predicate.Binop (res, Predicate.Plus, 
			Predicate.Binop (xi, Predicate.Times, Predicate.Var plain)),i+1)
	) (d, 0) plains in t	
	
(* Return templates excluding d part: *)
let get_coeffs template_expr = 
	let cs = ref [] in
	let _ = ignore(Predicate.map_expr (fun expr -> match expr with
		| Predicate.Binop (xi,Predicate.Times,_) -> ((cs := ((!cs)@[xi])); expr)
		| _ -> expr	
	) (Predicate.Atom (template_expr, Predicate.Ne, Predicate.PInt 0))) in
	(!cs)
	
(* In order to do sharing we simplify the index, removing the constants *)
let simplify_pexpr pexprs = 
	let rec simplify pexpr = match pexpr with
		| Predicate.PInt _ -> pexpr
	  | Predicate.Var _ -> pexpr
	  | Predicate.FunApp (fn, pexprs) -> Predicate.FunApp (fn, List.map simplify pexprs)
	  | Predicate.Binop (p1, rel, p2) -> (match (simplify p1, simplify p2) with
				| (Predicate.PInt p1, Predicate.PInt p2) -> Predicate.PInt 0
				| (Predicate.PInt p1, p2) -> p2
				| (p1, Predicate.PInt p2) -> p1
				| (p1, p2) -> Predicate.Binop (p1, rel, p2) 
			)
	  | Predicate.Field (n, pexpr) -> Predicate.Field (n, simplify pexpr)
	  | Predicate.Proj (n, pexpr) -> Predicate.Proj (n, simplify pexpr) in
	List.map (fun pexpr -> simplify pexpr) pexprs

(* return the known indices from all arrays other than arr *)		
let knonwn_indices arr_info arr' = 	
	let res = Hashtbl.fold (fun arr (_, indices) res -> match arr' with
		| Some arr' when (Path.same arr arr') -> res
		| _ -> res @ indices
	) arr_info [] in
	Common.remove_duplicates res
	
let is_readonly arr_info arr' = 
	Hashtbl.fold (fun arr (readonly, _) res -> 
		if (Path.same arr' arr) then readonly
		else res
	) arr_info false
	
let close_vars pred plains vars = 
	let closeset = Hashtbl.create (List.length vars) in
	let _ = List.iter (fun var -> Hashtbl.replace closeset var ()) vars in
	let domain set = Hashtbl.fold (fun v _ res -> res @ [v]) set [] in
	let rec loop n = 
		let closevars = domain closeset in
		(ignore (Predicate.map_pred (fun pred -> match pred with
			| Predicate.Atom _ -> 
				let allvars = Predicate.vars pred in
				let vars = List.filter (fun var -> 
					List.exists (fun plain -> Path.same var plain) plains) allvars in
				if (List.exists (fun var -> 
					List.exists (fun cvar -> Path.same var cvar) closevars) (vars) &&
					List.length allvars = List.length vars) then
					(List.iter (fun var -> Hashtbl.replace closeset var ()) (vars);
					Format.fprintf Format.std_formatter "In pred %a now with vars:" Predicate.pprint pred;
					List.iter (fun cvar -> Format.fprintf Format.std_formatter "%s " (Path.name cvar)) (domain closeset); 
					Format.fprintf Format.std_formatter "@."); pred
			| _ -> pred) pred); 
		if (Hashtbl.length closeset > n) then loop (Hashtbl.length closeset)) in
	(loop (Hashtbl.length closeset); domain closeset)
	
(* Find all variables that are not bounded in any array indices *)
let find_pure_plains plains arr_info = 
	List.filter (fun plain -> 
		List.for_all (fun index -> not (Path.same index plain)
		) (knonwn_indices arr_info (None))) plains
	
let get_template_class template = match template with
	| Predicate.Atom (_,Predicate.Eq,_) -> 0
	| Predicate.Atom (_,Predicate.Ne,_) -> 0
	| Predicate.Atom _ -> 1
	| _ -> 2 	

let find_index arr_info plains apath indices arr_cmps = 
	let plains = List.filter (fun plain -> 
		List.exists (fun index -> Path.same index plain) indices || 
			List.for_all (fun arr_cmp -> not (Path.same arr_cmp plain)) arr_cmps
	) plains in
	(* 1. the index of this array should not include indices from other arrays *)
	(* 2. the index of this array should not include indices from its own 
				if it is readlonly and its indices are not local *)
	List.filter (fun plain -> 
		List.exists (fun index -> Path.same index plain) indices ||
			List.for_all (fun index -> not (Path.same index plain)
		) (knonwn_indices arr_info apath)) plains					
										
let arrayname arr ex_assertion_flag = 
	if (ex_assertion_flag) then (Frame.old_array_flag ^ (Path.name arr))
	else Path.name arr
																																								
(** 1. if readonly array, a[ith] where ith is not local, plains should not include knonwn indices
    2. a[i], plains should not include knonwn indices from other b[..] where b != a
		3. a[j], where a is revisited, plains should be bounded to knonwn indices in this pred   *)		
(** coeff_constraints2 constrain the number of non-coeffs for user-defined assertion
    and only if user_assertion_flag is set we deal with coeff_constraints2 *)															
let generate_template arr_info arrs plains arr_indices arr_cmps pred 
	coeffs coeff_constraints coeff_constraints2 ds user_assertion_flag ex_assertion_flag = 	
	let plains = List.filter (fun p -> 
		not (String.compare (Path.name p) "r" = 0)	
	) plains in
	let _ = if (ex_assertion_flag) then assert (not (user_assertion_flag)) in	
	let _ = if (user_assertion_flag) then assert (not (ex_assertion_flag)) in
	let arrcells = Hashtbl.create 3 in (* Fixed for the same array *)
	let arrbounds = Hashtbl.create 3 in (* Fixed for the same array *)
	let arrindices = Hashtbl.create 3 in (* Not-fixed for a same array *)
	let arrlowerbounds = Hashtbl.create 3 in (* Fixed for the same array *)
	let arrexcells = Hashtbl.create 3 in (* Fixed for a same array? *)
	(* Optimization: if array index is a single variable then name it; if it appears as another array's
	   only one index then they share ...  *)
	let opt_bindings = Hashtbl.create 3 in 	
	(Predicate.map_expr (fun pexpr -> match pexpr with
		| Predicate.FunApp (f, args) when (String.compare f "UF" = 0) -> 
			let aname = List.hd args in
			let aind = simplify_pexpr (List.tl args) in
			(match aname with 
				| Predicate.Var apath when (List.exists (fun arr -> Path.same arr apath) arrs) ->
					if (Hashtbl.mem arrcells apath) then (* revisited array *)
						let cell = Hashtbl.find arrcells apath in
						let localplains = Hashtbl.find arr_indices apath in
						(* But the variable not bounded is excluded. Updated in 1/10 by He Zhu *)
						let plains = List.filter (fun localplain -> 
							List.exists (fun plain -> Path.same localplain plain) plains) localplains in
						(*if (ex_assertion_flag) then
							let excell = Path.mk_ident ("ex_" ^ (Path.name apath) ^ "_i") in
							let plains = find_index arr_info plains (Some apath) (Hashtbl.find arr_indices apath) arr_cmps in
							let (index,coeffs') = generate_arith_plains_template (plains@[excell]) coeffs ds in
							let _ = (coeff_constraints := [List.hd (List.rev coeffs')]::(!coeff_constraints)) in
							let (exbound, coeffs) = generate_arith_plains_template plains coeffs ds in
							let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in	
							((assert (not (Hashtbl.mem arrexcells apath)); Hashtbl.replace arrexcells apath (excell, exbound));
							assert (Hashtbl.mem arrindices apath);
							Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index]);
							Predicate.FunApp ((Frame.old_array_flag) ^ (Path.name apath), [index]))
						else*) 
						if (Hashtbl.mem opt_bindings aind && ((get_template_class pred != 1) || List.length plains = 0)) then 
							let (plains, _) = Hashtbl.find opt_bindings aind in
							(** To improve efficiency we really need to specially deal with user defined assertions *)
							if (user_assertion_flag) then
								let _ = assert (List.length (List.tl args) = 1) in
								let arg = List.hd (List.tl args) in
								let o_vars = Predicate.exp_vars arg in
								let _ = assert (List.length o_vars = 1) in
								let index = Predicate.exp_apply_substs [(List.hd o_vars, Predicate.Var cell)] arg in
								(assert (Hashtbl.mem arrindices apath);
								Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index]);
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
							else
								(*let index = generate_shared_arith_plains_template (plains@[cell]) coeffs ds in  -------- Double check*)
								let (index, coeffs) = generate_arith_plains_template (plains@[cell]) coeffs ds in
								let _ = (coeff_constraints := (*[List.hd (List.rev coeffs)]*)coeffs::(!coeff_constraints)) in	
								(assert (Hashtbl.mem arrindices apath);
								Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index]);
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
						else 
							let cands = 
								try Hashtbl.fold (fun key v res -> 
									let indvars = Predicate.exp_vars (List.hd aind) in
									let keyvars = Predicate.exp_vars (List.hd key) in
									if (List.exists (fun indvar -> 
										List.exists (fun keyvar -> Path.same indvar keyvar) keyvars) indvars) 
										then res @ [v] else res	
								) opt_bindings [] with _ -> [] in
							if (get_template_class pred == 0 && List.length cands > 0) then (* Explicitely reuse previous plains and coeffs. HACK. Fixme!! *)
								let (plains, _) = List.hd cands in
								let (index, coeffs) = generate_arith_plains_template (plains@[cell]) coeffs ds in
								let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in	
								(assert (Hashtbl.mem arrindices apath);
								Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index]);
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))		
							else (* the index of this array should be bounded to what appears in arr_indices *)
								let (index, coeffs) = generate_arith_plains_template (plains(*@[cell]*)) coeffs ds in
								let _ = (coeff_constraints := (*[List.hd (List.rev coeffs)]*)coeffs::(!coeff_constraints)) in	
								(assert (Hashtbl.mem arrindices apath);
								Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index]);
								(* store the coeffs so they can be shared  *)
								Hashtbl.replace opt_bindings (aind) (plains, coeffs); 
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
					else (* new array *)
						let indices = Hashtbl.find arr_indices apath in
						(** Let us firstly detect if apath can use another array's cell *)
						let related_arrs = Hashtbl.fold (fun arr arrcell res -> 
							let related_indices = Hashtbl.find arr_indices arr in
							if (List.exists (fun ri -> List.exists (fun i -> Path.same ri i) indices) related_indices) 
							then res@[arr] else res
						) arrcells [] in
						if (List.length related_arrs = 0) then (* create a new array *)
							(* isbase detect whether this array should be considered as a base *)
							let isbase = (Hashtbl.length arrcells = 0) in
							let isext = (Hashtbl.length arrexcells = 0) in
							if (isbase) then (* create the base *)
								let cell = Path.mk_ident ((Path.name apath) ^ "_i") in
								let plains = List.filter (fun plain -> 
									List.exists (fun index -> Path.same index plain) indices || 
										List.for_all (fun arr_cmp -> not (Path.same arr_cmp plain)) arr_cmps
								) plains in
								(* 1. the index of this array should not include indices from other arrays *)
								(* 2. the index of this array should not include indices from its own 
											if it is readlonly and its indices are not local *)
								let index_out_of_range = (
									List.exists (fun index -> List.for_all (fun plain -> not (Path.same index plain)) plains) indices
									) in
								let readonly = is_readonly arr_info apath in
								let plains = List.filter (fun plain -> 
									List.exists (fun index -> Path.same index plain) indices ||
										List.for_all (fun index -> not (Path.same index plain)
									) (knonwn_indices arr_info (if index_out_of_range && readonly then None else Some apath))) plains in
								(** To improve efficiency we really need to specially deal with user defined assertions *)	
								if (user_assertion_flag || (ex_assertion_flag && (not isext))) then 
									let _ = assert (index_out_of_range) in
									(* Fixme. The current implementation only considers one single dimension array *)
									let _ = assert (List.length (List.tl args) = 1) in
									let arg = List.hd (List.tl args) in
									let o_vars = Predicate.exp_vars arg in
									let _ = assert (List.length o_vars = 1) in
									let index = 
										if (ex_assertion_flag) then Predicate.Var cell
										else Predicate.exp_apply_substs [(List.hd o_vars, Predicate.Var cell)] arg in
									let (lowerbound, lowercoeffs) = 
										(*if (ex_assertion_flag) then generate_arith_plains_template_w_len_wo_d plains apath coeffs ds
										else*) generate_arith_plains_template_wo_d plains coeffs ds in
									let _ = (coeff_constraints2 := (lowercoeffs)::(!coeff_constraints2)) in
									let (higherbound, highercoeffs) = 
										if (ex_assertion_flag) then generate_arith_plains_template_w_len_wo_d plains apath coeffs ds
										else generate_arith_plains_template plains coeffs ds in
									let _ = 
										if (ex_assertion_flag) then (coeff_constraints2 := (highercoeffs)::(!coeff_constraints2)) in
									let _ = (coeff_constraints := (highercoeffs)::(!coeff_constraints)) in
									(*let _ = (coeff_constraints2 := (highercoeffs)::(!coeff_constraints2)) in*)
									(Hashtbl.replace arrcells apath cell;
									Hashtbl.replace arrbounds apath higherbound;
									Hashtbl.replace arrlowerbounds apath lowerbound;
									(*assert (not (Hashtbl.mem arrindices apath));
									Hashtbl.replace arrindices apath [index];*)
									if (Hashtbl.mem arrindices apath) then
										Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index])
									else Hashtbl.replace arrindices apath [index];
									Hashtbl.replace opt_bindings (aind) (plains, []);
									Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
								else if (ex_assertion_flag && isext) then  
									(** This is a bit unusual since we quantify its array index existentially *)
									let excell = Path.mk_ident ("ex_" ^ (Path.name apath) ^ "_i") in
									let plains = find_index arr_info plains (Some apath) indices arr_cmps in
									(*let (index, _) = generate_arith_plains_template_ex (plains) excell coeffs ds in
									(*let _ = (coeff_constraints := [List.hd (List.rev coeffs')]::(!coeff_constraints)) in	*)
									let (exbound, coeffs) = generate_arith_plains_template plains coeffs ds in
									let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in*)
									let index = Predicate.Var excell in
									let (exlowerbound, lowercoeffs) = generate_arith_plains_template_wo_d plains coeffs ds in
									let _ = (coeff_constraints2 := (lowercoeffs)::(!coeff_constraints2)) in
									let (exhigherbound, highercoeffs) = generate_arith_plains_template_w_len_wo_d plains apath coeffs ds in
									let _ = (coeff_constraints2 := (highercoeffs)::(!coeff_constraints2)) in
									let _ = (coeff_constraints := (highercoeffs)::(!coeff_constraints)) in
									((assert (not (Hashtbl.mem arrexcells apath)); Hashtbl.replace arrexcells apath (excell, exlowerbound, exhigherbound));
									assert (not (Hashtbl.mem arrindices apath));
									Hashtbl.replace arrindices apath ([index]);
									Predicate.FunApp ((Path.name apath), [index]))
								else
									let (index, coeffs') = generate_arith_plains_template (plains@[cell]) coeffs ds in
									let _ = (coeff_constraints := [List.hd (List.rev coeffs')]::(!coeff_constraints)) in	
									let (bound, coeffs) = generate_arith_plains_template plains coeffs ds in
									let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in
									(*let _ = if (user_assertion_flag) then (
										assert (index_out_of_range);
										(coeff_constraints2 := (List.tl (List.rev coeffs'))::(!coeff_constraints2))) in*)
									(Hashtbl.replace arrcells apath cell;
									Hashtbl.replace arrbounds apath bound;
									if (Hashtbl.mem arrindices apath) then
										Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index])
									else Hashtbl.replace arrindices apath [index];
									(* store the coeffs so they can be shared  *)
									Hashtbl.replace opt_bindings (aind) (plains, coeffs'); 
									Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
							(** The commented branch is buggy. No related array. But this is not based array *)
							(*else if (ex_assertion_flag) then 
								let excell = Path.mk_ident ("ex_" ^ (Path.name apath) ^ "_i") in
								let plains = find_index arr_info plains (Some apath) indices arr_cmps in
								let (index, coeffs') = generate_arith_plains_template (plains@[excell]) coeffs ds in
								let _ = (coeff_constraints := [List.hd (List.rev coeffs')]::(!coeff_constraints)) in	
								let (exbound, coeffs) = generate_arith_plains_template plains coeffs ds in
								let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in
								((assert (not (Hashtbl.mem arrexcells apath)); Hashtbl.replace arrexcells apath (excell, exbound));
								assert (not (Hashtbl.mem arrindices apath));
								Hashtbl.replace arrindices apath ([index]);
								Predicate.FunApp ((Frame.old_array_flag) ^ (Path.name apath), [index]))*)
							else (** the index of this array should be bounded to what appears in arr_indices **)
								let plains = Hashtbl.find arr_indices apath in
								let (index, coeffs) = generate_arith_plains_template plains coeffs ds in
								let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in
								(if (Hashtbl.mem arrindices apath) then 
									Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index])
								else Hashtbl.replace arrindices apath [index];
								(* sotre the coefs so they can be shared *)
								Hashtbl.replace opt_bindings aind (plains, coeffs);
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
						(** The related array set is not empty: the following commented branch is buggy *)		
						(*else if (ex_assertion_flag) then
							let excell = Path.mk_ident ("ex_" ^ (Path.name apath) ^ "_i") in
							let plains = find_index arr_info plains (Some apath) indices arr_cmps in
							let (index, coeffs') = generate_arith_plains_template (plains@[excell]) coeffs ds in
							let _ = (coeff_constraints := [List.hd (List.rev coeffs')]::(!coeff_constraints)) in	
							let (exbound, coeffs) = generate_arith_plains_template plains coeffs ds in
							let _ = (coeff_constraints := coeffs::(!coeff_constraints)) in
							((assert (not (Hashtbl.mem arrexcells apath)); Hashtbl.replace arrexcells apath (excell, exbound));
							assert (not (Hashtbl.mem arrindices apath));
							Hashtbl.replace arrindices apath ([index]);
							Predicate.FunApp ((Frame.old_array_flag) ^ (Path.name apath), [index]))*)
						else (* create a new array but share its cell with existing arrays *)
							let related_arr = List.hd related_arrs in
							let cell = Hashtbl.find arrcells related_arr in
							let plains = List.filter (fun plain -> 
								List.exists (fun index -> Path.same index plain) indices || 
									List.for_all (fun arr_cmp -> not (Path.same arr_cmp plain)) arr_cmps
							) plains in
							(* the index of this array should not include indices from other arrays *)
							let plains = List.filter (fun plain -> 
								List.exists (fun index -> Path.same index plain) indices ||
									List.for_all (fun index -> not (Path.same index plain)
								) (knonwn_indices arr_info (Some apath))) plains in
							if (Hashtbl.mem opt_bindings aind) then 
								let (plains, coeffs) = Hashtbl.find opt_bindings aind in
								let index = generate_shared_arith_plains_template (plains@[cell]) coeffs ds in
								(if (Hashtbl.mem arrindices apath) then
									Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index])
								else Hashtbl.replace arrindices apath [index];
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
							else 
								let (index, coeffs) = generate_arith_plains_template (plains@[cell]) coeffs ds in
								let _ = (coeff_constraints := [List.hd (List.rev coeffs)]::(!coeff_constraints)) in	
								(if (Hashtbl.mem arrindices apath) then
									Hashtbl.replace arrindices apath ((Hashtbl.find arrindices apath)@[index])
								else Hashtbl.replace arrindices apath [index];
								(* store the coeffs so they can be shared  *)
								Hashtbl.replace opt_bindings (aind) (plains, coeffs); 
								Predicate.FunApp (arrayname apath ex_assertion_flag, [index]))
				| _ -> pexpr)
		| _ -> pexpr
	) pred, arrexcells, arrcells, arrlowerbounds, arrbounds, arrindices, 
	coeffs, coeff_constraints, coeff_constraints2, ds, user_assertion_flag, ex_assertion_flag)

(** Over-approximate array update 
  1. If set a[i] to an integer or a variable then no approximation 
	2. If set a[i] to b[f] where f ranges over i then no approximation
	3. If set a[i] to b[f] whre f does not range over i then approximate it*)		
let approximate_array_update pred template = match (pred, template) with
	| (Predicate.Atom (p1, Predicate.Eq, (Predicate.FunApp (fname2, args2) as p2)), 
		Predicate.Atom (t1, Predicate.Eq, t2)) when (String.compare fname2 "UF" = 0) -> 
		let functions = Predicate.get_all_funs pred in
		let functions = List.filter (fun f -> (f <> p2)) functions in
		if (List.length functions = 0) then template (*case 1*)
		else 
			let flag = List.exists (fun f -> match f with
				| Predicate.FunApp (fname1, args1) when (String.compare fname1 "UF" = 0)->
					let args1 = List.tl args1 in
					let args2 = List.tl args2 in 
					List.for_all2 (fun arg1 arg2 -> 
						let vars1 = Predicate.exp_vars arg1 in
						let vars2 = Predicate.exp_vars arg2 in
						List.for_all (fun var2 -> List.exists (fun var1 -> Path.same var2 var1) vars1) vars2	
					) args1 args2
				| _ -> assert false) functions in
			if (flag) then template (*case 2*)
			else 
				if (List.exists (fun f -> match f with
					| Predicate.FunApp (fname1, args1) -> 
						(let fn1 = List.hd args1 in
						let fn2 = List.hd args2 in
						fn1 = fn2)
					| _ -> assert false) functions) then (* Fixme for fixable incompleteness *) 
					(Predicate.Atom (t1, Predicate.Le, t2)) (*if a = b then we prefer a >= b *)
				else (Predicate.Atom (t1, Predicate.Ge, t2))  (*if a <> b* then we prefer a <= b*) (*case 3*)
	| _ -> (*Format.fprintf Format.std_formatter "pred is %a@." Predicate.pprint pred; assert false*) template

let identify_user_assertion pred plains =
	let indices = ref [] in
	let _ = Predicate.map_expr (fun expr -> match expr with
		| Predicate.FunApp (fname, args) when (String.compare "UF" fname = 0) ->
			let index = List.fold_left (fun res arg -> res @ (Predicate.exp_vars arg)) [] (List.tl args) in
			(indices := (!indices)@index; expr) 
		| _ -> expr) pred in
	let indices = Common.remove_duplicates (!indices) in
	(List.length indices > 0) &&
	List.for_all (fun index -> List.for_all (fun plain -> not (Path.same index plain)) plains) indices
	
let identify_ex_assertion ispost pred plains = 
	if (ispost) then
		if (identify_user_assertion pred plains) then false
		else (match pred with
			| Predicate.Atom (Predicate.FunApp _, Predicate.Eq, Predicate.FunApp _) -> true
			| _ -> false)
	else false
	
let get_var expr = match expr with Predicate.Var var -> var | _ -> assert false	
	
let identify_arr_to_arr effcons = (* fixme to cut off branches *)
	let arr_to_arr = ref [] in
	(ignore (Predicate.map_pred (fun pred -> match pred with
		| Predicate.Atom (p1, Predicate.Eq, (Predicate.FunApp (fname2, args2) as p2)) 
		when (String.compare fname2 "UF" = 0) -> 
			let functions = Predicate.get_all_funs pred in
			let functions = List.filter (fun f -> (f <> p2)) functions in
			if (List.length functions = 0) then pred
			else 
				let fn2 = get_var (List.hd args2) in
				(List.iter (fun f -> match f with
					| Predicate.FunApp (fname1, args1) when (String.compare fname1 "UF" = 0) ->
						let fn1 = get_var (List.hd args1) in
						if (Path.same fn1 fn2) then ()
						else (arr_to_arr := (fn2, fn1)::(!arr_to_arr))
					| _ -> assert false) functions; pred)
		| _ -> pred
	) effcons); Common.remove_duplicates (!arr_to_arr)) 	

(** drop predicates if unknown array is contained *)
let invalid_pred pred arrs = 
	let flag = ref false in
	(ignore (Predicate.map_expr (fun expr -> 
		if (!flag) then expr
		else match expr with
			| Predicate.FunApp (fn, args) when (String.compare "UF" fn = 0) ->
				let fn = get_var (List.hd args) in
				if (List.for_all (fun arr -> not (Path.same arr fn)) arrs) then
					(flag := true; expr)
				else expr 
			| _ -> expr 
	) pred); !flag) 
	
let verydiff pred = (match pred with
	| Predicate.Atom (p1, Predicate.Eq, (Predicate.FunApp (fname2, args2) as p2)) 
		when (String.compare fname2 "UF" = 0) ->
		let functions = Predicate.get_all_funs pred in
		let functions = List.filter (fun f -> (f <> p2)) functions in
		if (List.length functions = 0) then false
		else 
			List.exists (fun f -> match f with
				| Predicate.FunApp (fname1, args1) when (String.compare fname1 "UF" = 0)->
					(List.hd args1 <> List.hd args2) &&
					(let args1 = List.tl args1 in
					let args2 = List.tl args2 in 
					List.for_all2 (fun arg1 arg2 -> 
						let vars1 = Predicate.exp_vars arg1 in
						let vars2 = Predicate.exp_vars arg2 in
						List.for_all (fun var2 -> List.for_all (fun var1 -> not (Path.same var2 var1)) vars1) vars2	
					) args1 args2)
				| _ -> assert false) functions
	| _ -> (*Format.fprintf Format.std_formatter "pred is %a@." Predicate.pprint pred; assert false*)false)

(** Check the template's wellformness *)
let wellformed template ds =
	let functions = Predicate.get_all_funs template in
	let ds = Hashtbl.fold (fun d _ res -> d::res) ds [] in
	List.for_all (fun f -> match f with
		| Predicate.FunApp (_, args) -> 
			List.for_all (fun arg -> (* Simply check the argument cannot be bind to a single constant instance *)	
				not (List.exists (fun d -> d = arg) ds)
			) args
		| f -> assert false
	) functions
	
let get_all_cmps arrs pred = 
	let cmps = ref [] in
	(ignore (Predicate.map_pred (fun pred -> match pred with
		| Predicate.Atom (p1, rel, p2) when (not (invalid_pred pred arrs)) ->
			if (List.exists (fun var -> List.exists (fun arr -> Path.same var arr) arrs) (Predicate.vars pred)) then
				let (_, ccmps) = preprocess_template arrs pred in
				let ccmps = Hashtbl.fold (fun k _ res -> res @ [k]) ccmps [] in
				(cmps := (!cmps)@ccmps; pred)
			else pred
		| pred -> pred
	) pred); Common.remove_duplicates (!cmps))	

(**
 if the pred used to generate template is indentified as a user-defined predicate,
 for genenerating precondition, we do substitution
 for genenerating postcondition, we also do reverse subsitution
 *)											
let infer_template ispost effcons symb_pre arrs plains allplains = 
	let arr_info = Hashtbl.create (List.length arrs) in
	let _ = collect_arr_info effcons symb_pre arrs arr_info in
	let arr_to_arr = identify_arr_to_arr effcons in
	let templates = ref [] in
	(ignore (Predicate.map_pred (fun pred -> match pred with
		| Predicate.Atom (p1, rel, p2) when (not (invalid_pred pred arrs)) -> 
			(*******We deal with corner cases for user defined assertion********)
			let user_assertion_flag = identify_user_assertion pred allplains in
			(*******We deal with corner cases for array preservation************)
			let ex_assertion_flag = identify_ex_assertion ispost pred allplains in
			let candidates = 
				if (user_assertion_flag) then 
					if (List.length arr_to_arr > 0) then
						if (ispost) then 
							let subs = List.map (fun (p, q) -> (q, Predicate.Var p)) arr_to_arr in
							(*let _ = (Format.fprintf Format.std_formatter "Transform %a because %a@."
								Predicate.pprint pred Frame.pprint_sub (List.hd subs);
								) in*)
							[(Predicate.exp_apply_substs subs p1, Predicate.exp_apply_substs subs p2, rel)]
						else 
							let subs = List.map (fun (p, q) -> (p, Predicate.Var q)) arr_to_arr in
							(*let _ = (Format.fprintf Format.std_formatter "Transform %a because %a@."
								Predicate.pprint pred Frame.pprint_sub (List.hd subs);
								) in*)
							let subs' = List.map (fun (p, q) -> (q, Predicate.Var p)) arr_to_arr in
							[(Predicate.exp_apply_substs subs p1, Predicate.exp_apply_substs subs p2, rel);
							(Predicate.exp_apply_substs subs' p1, Predicate.exp_apply_substs subs' p2, rel)]
					else [(p1, p2, rel)]
				else [(p1, p2, rel)] in
			(*********** Now begin functional code  **************)
			(List.iter (fun (p1, p2, rel) -> 
				let n_pred = Predicate.Atom (p1, rel, p2) in
				(let varset1 = Predicate.exp_vars p1 in
				let varset2 = Predicate.exp_vars p2 in
				if (List.exists (fun var -> 
					List.exists (fun arr -> Path.same arr var) arrs) (varset1 @ varset2)) then
					(* Creating a template *)
					(** A store of co-efficients and their value should be -1, 0, 1 only *)
					let coeffs = Hashtbl.create 13 in
					(* Each element is a set; and cannot all elements in this set is assigned to 0 *)
					let coeff_constraints = ref [] in
					(* Each element is a set; and at most one of the elements is assigned to 0  *)
					let coeff_constraints2 = ref [] in
					(** A store of constant and their value should be bounded *)
					let ds = Hashtbl.create 13 in
					(* Information from symb_pre *)
					let (arr_indices, arr_cmps) = preprocess_template arrs n_pred in      
					let arr_cmps = Hashtbl.fold (fun v _ res -> res @ [v]) arr_cmps [] in 
					if (List.for_all (fun cmp -> List.exists (fun plain -> Path.same cmp plain) plains) arr_cmps) 
					then (* updated in 1/10 by He Zhu. We dont want arr_cmps contains unbounded variables *)	
						if (!Clflags.preservation) then
							let arr_cmps = get_all_cmps arrs symb_pre in
							(templates := 
								if (ex_assertion_flag) then
									(pred,
									generate_template arr_info arrs plains arr_indices arr_cmps n_pred 
										coeffs coeff_constraints coeff_constraints2 ds user_assertion_flag true)
									:: (!templates)
								else (!templates))
						else
							(templates := 
								if (ex_assertion_flag) then 
									(*(pred,
									generate_template arr_info arrs plains arr_indices arr_cmps n_pred 
										coeffs coeff_constraints coeff_constraints2 ds user_assertion_flag true)*)
									((pred, 
									generate_template arr_info arrs plains arr_indices arr_cmps n_pred 
										coeffs coeff_constraints coeff_constraints2 ds user_assertion_flag false)
									::(!templates))
								else
								(pred, 
								generate_template arr_info arrs plains arr_indices arr_cmps n_pred 
									coeffs coeff_constraints coeff_constraints2 ds user_assertion_flag false)
								::(!templates))
					else ()
				else (* Incompleteness arises from here. Since we just ignore this info *)())
			) candidates; pred)
		| _ -> pred 
	) (Predicate.And (effcons, symb_pre))); 
	(** Post-process of templates *)
	Common.map_partial (fun (pred, ((template,excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag) as t)) ->
		let _ = Format.fprintf Format.std_formatter "Note predicate: %a@. yiels tempalte: %a@." Predicate.pprint pred Predicate.pprint template in
		(** Check the wellformness of the template *)
		if (wellformed template ds) then
			let n = ref 0 in (* the number of times that predicate show up in effcons *)
			let _ = ignore(Predicate.map_pred (fun pred' -> match pred' with
				| Predicate.Atom (_,Predicate.Eq,_) ->(if (pred = pred') then (n := 1+(!n)); pred')
				| _ -> pred'
			) effcons) in
		
			(** We want to add more constraints for another array *)
			if (!n > 0) then match template with
				| Predicate.Atom (p1, Predicate.Eq, p2) ->
					if (verydiff pred) then
						if (ex_assertion_flag) then (** Support array preservation property *)
							(assert (Hashtbl.length excells > 0);
							Some (template,excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag))
						else 
							let vars = Predicate.exp_vars p1 in
							let vars = List.filter (fun var -> List.exists (fun plain -> Path.same var plain) plains) vars in
							let rightvars = close_vars effcons plains (Common.remove_duplicates vars) in
							let leftvars = find_pure_plains plains arr_info in
							(* minus vars to avoid uninteresting invariants *)
							let rightvars = List.filter (fun rv -> List.for_all (fun var -> not (Path.same rv var)) vars) rightvars in
							let leftvars = List.filter (fun lv -> List.for_all (fun var -> not (Path.same lv var)) vars) leftvars in
							let leftvars = List.filter (fun lv -> List.for_all (fun var -> not (Path.same lv var)) rightvars) leftvars in 
							if (List.length rightvars > 0 && List.length vars > 0) then 
								let (middletemplate,effs) = generate_arith_plains_template_wo_d vars coeffs ds in
								let _ = (coeff_constraints := effs::(!coeff_constraints)) in
								let (lefttemplate, effs) = 
									if (List.length leftvars > 0) then
										generate_arith_plains_template_wo_d leftvars coeffs ds 
									else assert false in
								let (righttemplate, effs) = generate_arith_plains_template_wo_d rightvars coeffs ds in
								let _ = (coeff_constraints := effs::(!coeff_constraints)) in
								
								let premise = Predicate.And (Predicate.Atom (lefttemplate,Predicate.Le,middletemplate), 
															Predicate.Atom (middletemplate,Predicate.Lt,righttemplate)) in
								let condition = Predicate.big_and [Predicate.Atom (Predicate.PInt 0, Predicate.Le, lefttemplate);
																Predicate.Atom (lefttemplate, Predicate.Le, middletemplate);
																Predicate.Atom (middletemplate, Predicate.Le, righttemplate)] in
								
								let template = approximate_array_update pred template in
								let template = Predicate.implies (premise, template) in
								Some (Predicate.And (condition, template),excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag)
							else Some (approximate_array_update pred template,excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag) 
					else
						if (ex_assertion_flag) then 
							(assert (Hashtbl.length excells > 0);
							Some (template,excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag))
						else
							Some (approximate_array_update pred template,excells,a,b',b,c,coeffs,coeff_constraints,f,ds,g,ex_assertion_flag)
				| _ -> (Format.fprintf Format.std_formatter "%a" Predicate.pprint template;assert false) (*FIXME!!guards discareded*)
			else Some t
		else None
		(** End Here *)
		
		(*if (!n > 1) then (* guards should be conjoined with the template *)
			let vars = Predicate.vars pred in
			let vars = List.filter (fun var -> List.exists (fun plain -> Path.same var plain) plains) vars in
			let vars = close_vars effcons plains (Common.remove_duplicates vars) in
			let (template',effs) = generate_arith_plains_template vars coeffs ds in
			let _ = (coeff_constraints := effs::(!coeff_constraints)) in
			let condition = Predicate.Atom (template', Predicate.Le, Predicate.PInt 0) in
			match template with (* Fix me. How to overapproximate array update *)
				| Predicate.Atom (p1, Predicate.Eq, p2) -> 
					let template = (*Predicate.Atom (p1, Predicate.Ge, p2)*) approximate_array_update pred template in
					let template = Predicate.implies (Predicate.Atom (template', Predicate.Lt, Predicate.PInt 0), template) in
					Some (Predicate.And (condition, template),a,b',b,c,coeffs,coeff_constraints,f,ds,g)
				| _ -> (Format.fprintf Format.std_formatter "%a" Predicate.pprint template;assert false) (*FIXME!!guards discareded*)
		else if (!n = 1) then
			Some (approximate_array_update pred template,a,b',b,c,coeffs,coeff_constraints,f,ds,g)
		else Some t*)
	) (!templates))	
	
let fst (a,b,c) = a
	
let equal_template t1 t2 = match (t1, t2) with
	| ((pred1,excell1,cell1,_,_,_,coeffs1,_,_,ds1,_,_), (pred2,excell2,cell2,_,_,_,coeffs2,_,_,ds2,_,_)) -> 
		if (Hashtbl.length excell1 = Hashtbl.length excell2 &&
			  Hashtbl.length cell1 = Hashtbl.length cell2 && 
				Hashtbl.length coeffs1 = Hashtbl.length coeffs2 &&
				Hashtbl.length ds1 = Hashtbl.length ds2) then
			let vars1 = 
				(Hashtbl.fold (fun _ excell res -> res@[fst excell]) excell1 []) @
				(Hashtbl.fold (fun _ cell res -> res@[cell]) cell1 []) @ 
				(Hashtbl.fold (fun v _ res -> 
					res@[match v with Predicate.Var v -> v | _ -> assert false]) (coeffs1) []) @ 
				(Hashtbl.fold (fun v _ res -> 
					res@[match v with Predicate.Var v -> v | _ -> assert false]) (ds1) []) in
			let vars2 = 
				(Hashtbl.fold (fun _ excell res -> res@[fst excell]) excell2 []) @
				(Hashtbl.fold (fun _ cell res -> res@[cell]) cell2 []) @ 
				(Hashtbl.fold (fun v _ res -> 
					res@[match v with Predicate.Var v -> v | _ -> assert false]) (coeffs2) []) @ 
				(Hashtbl.fold (fun v _ res -> 
					res@[match v with Predicate.Var v -> v | _ -> assert false]) (ds2) []) in
			let vars1 = List.sort (fun v1 v2 -> String.compare (Path.unique_name v1) (Path.unique_name v2)) vars1 in
			let vars2 = List.sort (fun v1 v2 -> String.compare (Path.unique_name v1) (Path.unique_name v2)) vars2 in
			let (subs1, subs2,_) = List.fold_left2 (fun (ressubs1, ressubs2, i) var1 var2 -> 
				let dummyvar = Predicate.Var (Path.mk_ident ("dummy"^(string_of_int i))) in
				(ressubs1@[(var1, dummyvar)], ressubs2@[(var2, dummyvar)], i+1)
			) ([],[],0) vars1 vars2 in
			let (pred1, pred2) = (Predicate.apply_substs subs1 pred1, Predicate.apply_substs subs2 pred2) in
			(*let _ = Format.fprintf Format.std_formatter "pred1=%a@. while pred2=%a@." Predicate.pprint pred1 Predicate.pprint pred2 in*)
			(pred1 = pred2)
		else false
		
let opt_plains samples = 
	let plaindata = Hashtbl.create 7 in
	let _ = List.iter (fun sample -> 
		Hashtbl.iter (fun p c -> match c with
			| Cell d -> 
				if (Hashtbl.mem plaindata p) then
					Hashtbl.replace plaindata p ((Hashtbl.find plaindata p)@[d])
				else Hashtbl.replace plaindata p [d]
			| Cells _ -> ()
		) sample	
	) samples in
	(* Strategy: Sort plaindata and find how value increments or decrements *)
	let diffs = Hashtbl.create (Hashtbl.length plaindata) in
	let _ = Hashtbl.iter (fun p ds -> 
		let ds = Common.remove_duplicates ds in
		let ds = List.sort (fun x y -> Pervasives.compare x y) ds in
		let (diff, _) = List.fold_left (fun (res, prev) d -> 
			(res @ [(d-prev)], d)
		) ([], List.hd ds) (List.tl ds) in
		Hashtbl.replace diffs p diff
	) plaindata in
	let _ = Hashtbl.iter (fun p ds -> 
		let ds = List.sort (fun x y -> Pervasives.compare x y) ds in
		Format.fprintf Format.std_formatter "%s -->" (Path.name p);
		List.iter (fun d -> Format.fprintf Format.std_formatter "%d " d) ds;
		Format.fprintf Format.std_formatter "@." 
	) diffs in
	(** A simple heuristic strategy: *)
	let candidates = Hashtbl.fold (fun p ds res -> 
		if (List.for_all (fun d -> d = 1) ds) then res @ [(p,ds)]
		else res
	) diffs [] in
	let maxlen = List.fold_left (fun res (p, ds) -> 
		(*let ds = Common.remove_duplicates ds in*)
		if (List.length ds) > res then (List.length ds)
		else res
		) (-1) candidates in
	if (maxlen < 0) then assert false 
	else 
		List.fold_left (fun res (p, ds) -> 
			(*let ds = Common.remove_duplicates ds in*)
			if (abs(List.length ds - maxlen) <= 1) then res@[p]
			else res
		) [] candidates
			
(* Mine array invariant from positives only *) 
let mine_template ispost goods names env fr effcons symb_pre =	
	let allbindings = Frame.get_fun_bindings env fr in
	let tbl = Hashtbl.create 5 in
	let _ = List.iter (fun (p, f) -> Hashtbl.replace tbl (Path.name p) p) allbindings in
	let (arrs, plains) = List.fold_left (fun (resarrs, resplains) (p, f) -> match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_unit -> (resarrs, resplains)
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> 
			(** check whether adjacent elements of p is constrainted in badf *)
			if (List.exists (fun name -> Common.str_contains name (Path.name p)) names) 
			then (resarrs @ [p], resplains) else (resarrs, resplains)
		|	Frame.Fconstr (x,_,_,_,_) when x = Predef.path_int -> 
			if (List.exists (fun name -> String.compare (Path.name p) name = 0) names) 
			then (resarrs, resplains @ [p]) else (resarrs, resplains)
		| Frame.Farrow _ -> (resarrs, resplains)
		| _ -> (Format.fprintf Format.std_formatter "Unrecognized Frame %a for %s@." 
						Frame.pprint f (Path.name p); 
						assert false)
	) ([], []) allbindings in
	if (List.length arrs = 0) then ([], [])
	else
	let samples = transel_sample ispost goods names arrs tbl in
	(** !! If verifying post-condition we do an optimization: iterative counters are removed.
		However, if thery are the only plains we should keep them *)
	let allplains = plains in
	let iters = 
		if (ispost) then opt_plains samples else [] in
	let plains = 
		if (ispost) then
			let _ = List.iter (fun iter -> Format.fprintf Format.std_formatter "iter=%s@." (Path.name iter)) iters in
			let res = List.filter (fun plain -> List.for_all (fun iter -> not (Path.same iter plain)) iters) plains in
			if (List.length res = 0) then plains
			else res
		else plains in
	let templates = infer_template ispost effcons symb_pre arrs plains allplains in
	let templates = Common.remove_customized_duplicates (fun t1 t2 -> equal_template t1 t2) templates in
	(*let templates = List.map (fun (template,a,b,c,d,e,f) -> match template with
		| Predicate.Atom (p1, Predicate.Gt, p2) -> (Predicate.Atom (p1, Predicate.Ge, p2),a,b,c,d,e,f)
		| Predicate.Atom (p1, Predicate.Lt, p2) -> (Predicate.Atom (p1, Predicate.Lt, p2),a,b,c,d,e,f)
		| _ -> (template,a,b,c,d,e,f)
	) templates in*)
	(*let templates = List.map (fun (template,a,b,c,d,e,f) -> match template with
		| Predicate.Atom (p1, Predicate.Eq, p2) -> (Predicate.Atom (p1, Predicate.Ge, p2),a,b,c,d,e,f)
		| Predicate.And (Predicate.Atom (p1, Predicate.Eq, p2), guards) -> 
			(Predicate.And (Predicate.Atom (p1, Predicate.Ge, p2), guards),a,b,c,d,e,f)
		| _ -> (template,a,b,c,d,e,f)
	) templates in *)
	let _ = List.iter (fun (t,_,_,_,_,_,coeffs,_,_,_,_,_) -> 
		Format.fprintf Format.std_formatter "template:%a with coeffs size as%d@." Predicate.pprint' t (Hashtbl.length coeffs)) templates in
	
	(*let templates = List.filter (fun (t,_,_,_,coeffs,_,_) -> (Hashtbl.length coeffs <> 13)) templates in*)
	(** We remove invalid template with a function argument just as "d" which represents an integer and bounds just ad "d" as well *)
	let templates = List.filter (fun (t,_,_,_,bounds,_,_,_,_,_,_,_) -> 
		let functions = Predicate.get_all_funs t in
		List.for_all (fun f -> match f with
			| Predicate.FunApp (_, args) -> 
				if (List.length args <> 1) then true
				else (match (List.hd args) with
					| Predicate.Var arg -> not (String.compare (Path.name arg) "d"= 0)
					| arg -> true
				)
			| f -> true
		) functions && List.for_all (fun b -> match b with
			| Predicate.Var arg -> not (String.compare (Path.name arg) "d"= 0) 
			| b -> true
		) (Hashtbl.fold (fun _ b res -> res @ [b]) bounds [])
	) templates in
	(************* Begin to work with each template ******************)	
	(************* Begin to work with each template ******************)	
	(iters, List.flatten (List.map (
		fun (template, arrexcells, arrcells, arrlowerbounds, arrbounds, arrindices, 
					coeffs, coeff_constraints, coeff_constraints2, ds, user_assertion_flag, ex_assertion_flag) -> 
		(** Prepare for arrays *)
		let arrindcons = Hashtbl.fold (fun arr indices res -> 
			(List.fold_left (fun res arrind -> 
				res @ [Predicate.And (
					Predicate.Atom (Predicate.PInt 0, Predicate.Le, arrind),
					Predicate.Atom (arrind, Predicate.Lt, Predicate.FunApp ("Array.length", [Predicate.Var arr]))
			)]) res indices) @ (snd (List.fold_left (fun (resinds, respreds) arrind ->
				(** Ignore index for existential facts. Fix me? *)
				let arrindvars = Predicate.exp_vars arrind in
				if (List.exists (fun indvar -> Common.str_contains (Path.name indvar) "ex_") arrindvars) then (resinds, respreds)
				else 
					let preds = List.map (fun resind -> Predicate.Atom (resind,Predicate.Ne,arrind)) resinds in
					(resinds @ [arrind], respreds @ preds)
			) ([], []) indices))
		) arrindices [] in
		(* Cannot all coeffs in bounds are 0-----Imp! *)
		(*let bounds = List.map (fun arr -> 
			let (t, coeffs) = generate_arith_plains_template plains in
			((coeff_constraints := coeffs::(!coeff_constraints)); t)) arrs in*)
		let simple_template = (Predicate.implies (
			(let premise = Predicate.big_and (Hashtbl.fold (fun arr arrcell res -> 
				let arrbound = Hashtbl.find arrbounds arr in
				res @ [Predicate.And (
					(if (user_assertion_flag || ex_assertion_flag) then
						let _ = assert (Hashtbl.mem arrlowerbounds arr) in
						Predicate.Atom (Hashtbl.find arrlowerbounds arr, Predicate.Le, Predicate.Var arrcell)
					else
						Predicate.Atom (Predicate.PInt 0, Predicate.Le, Predicate.Var arrcell)), 
					Predicate.Atom (Predicate.Var arrcell, Predicate.Lt, arrbound))]	
			) arrcells []) in
			premise),
			(let expremise = 
				Predicate.big_and (Hashtbl.fold (fun arr (excell, exlowerbound, exhigherbound) res ->
				res @ [Predicate.And (Predicate.Atom (exlowerbound, Predicate.Le, Predicate.Var excell),
					Predicate.Atom (Predicate.Var excell, Predicate.Lt, exhigherbound))]	
				) arrexcells []) in
			match template with
				| Predicate.And (_, ((Predicate.Or _) as t)) -> 
					if (ex_assertion_flag) then Predicate.And (expremise, t) else t
				| t -> if (ex_assertion_flag) then Predicate.And (expremise, t) else t)
			)) in	
		let template = Predicate.implies (
			(let premise = Predicate.big_and (Hashtbl.fold (fun arr arrcell res -> 
				let arrbound = Hashtbl.find arrbounds arr in
				res @ [Predicate.And (
					(if (user_assertion_flag || ex_assertion_flag) then
						Predicate.Atom (Hashtbl.find arrlowerbounds arr, Predicate.Le, Predicate.Var arrcell)
					else
					 Predicate.Atom (Predicate.PInt 0, Predicate.Le, Predicate.Var arrcell)), 
					Predicate.Atom (Predicate.Var arrcell, Predicate.Lt, arrbound))]	
			) arrcells []) in
			premise),
			(** How to insert array bound constraints is tailored *)
			(let expremise = 
				Predicate.big_and (Hashtbl.fold (fun arr (excell, exlowerbound, exhigherbound) res ->
				res @ [Predicate.And (Predicate.Atom (exlowerbound, Predicate.Le, Predicate.Var excell),
					Predicate.Atom (Predicate.Var excell, Predicate.Lt, exhigherbound))]	
				) arrexcells []) in
			match template with
				| Predicate.Atom _ -> (** very simple, most used template *)
					let t = Predicate.And (
						Predicate.big_and arrindcons,
						template
						(*Predicate.Atom (
							List.fold_left (fun res arrvalue -> 
								Predicate.Binop (arrvalue, Predicate.Plus, res)	
							) (fst (generate_arith_plains_template plains)) arrvalues,
							Predicate.Ge,
							Predicate.PInt 0
					)*)) in
					if (ex_assertion_flag) then Predicate.And (expremise, t) else t
				| Predicate.And (g, Predicate.Or (Predicate.Not g', t)) ->
					(** a bit complex, templates conjoined with guards *)
					let t = Predicate.And (g, Predicate.Or (Predicate.Not g', 
						Predicate.And (Predicate.big_and arrindcons, t))) in
					if (ex_assertion_flag) then Predicate.And (expremise, t) else t
				| _ -> assert false)
		) in
		let _ = Format.fprintf Format.std_formatter "template=%a@." Predicate.pprint' template in
		(*type value = Cell of int | Cells of (string * value) list list
			type sample = (string, value) Hashtbl.t	*)
		(* For each sample we obtained, we instantiate the template using it *)	
		let predicates = List.map (fun sample -> 
			let (subs, arrcons) = Hashtbl.fold (fun var values (res_subs, res_arrcons) -> 
				(** If this is oldarray, skip it if ex_assertion_flag is not set *)
				if (not ex_assertion_flag && (Common.str_contains (Path.name var) Frame.old_array_flag)) 
				then (res_subs, res_arrcons) else
				match values with
					| Cell value -> 
						((var, Predicate.PInt value)::res_subs, res_arrcons)
					| Cells values -> 
						let resultname = 
							if (Common.str_contains (Path.name var) Frame.old_array_flag) then
								String.sub (Path.name var) (String.length Frame.old_array_flag) 
									(String.length (Path.name var) - String.length Frame.old_array_flag)
							else Path.name var in
						let arrcons = List.map (fun values -> (* Do a simple scan to remove unecessary dumping for this phase *)
							let values = List.filter (fun (n, _) ->
								let valid_arr_value = resultname^"_"^return_repr in (* Must Fixme!! *)
								let valid_arr_domains = Array.to_list (Array.init 9 (fun i -> resultname^"_"^(string_of_int i))) in
								List.exists (fun valid -> String.compare valid n = 0) (valid_arr_value::valid_arr_domains)
							) values in
							let arrvalue = snd (List.hd (List.rev values)) in
							let arrranges = List.map snd (List.rev (List.tl (List.rev values))) in
							match arrvalue with
								| Cell value -> (
									let arrranges = List.map (fun value -> match value with
										| Cell value -> Predicate.PInt value
										| _ -> (* Not supported yet *) assert false
									) arrranges in
									Predicate.Atom (Predicate.FunApp (Path.name var, arrranges), Predicate.Eq, Predicate.PInt value)
								)
								| _ -> (* Not supported yet *) assert false	
						) values in
						let arrlen = List.length values in
						let arrcons = (Predicate.Atom (
							Predicate.FunApp ("Array.length", [Predicate.Var var]), Predicate.Eq, Predicate.PInt arrlen)
							)::arrcons in
						let res_subs = 
							if (ex_assertion_flag) then 
								res_subs @ (* len_a is substituted with its actual *)
								[((Path.Pident (Ident.create_with_stamp ("len_"^(Path.name var)) (Path.stamp var))), Predicate.PInt arrlen)]
							else res_subs in
						(res_subs, arrcons@res_arrcons)
			) sample ([], []) in
			let rec expand_template i arrs arrcells res =
				let n = List.length arrs in 
				if (i < n) then
					let arr = List.nth arrs i in
					let arrsample = Hashtbl.find sample arr in
					let size = match arrsample with Cells values -> List.length values | _ -> assert false in
					let ind = List.nth arrcells i in
					let res = 
						if (i > 0) then	
							List.flatten (List.map (fun resele -> 
								(Array.to_list (Array.init size (fun i -> resele@[(ind, Predicate.PInt i)])))
							) res)
						else Array.to_list (Array.init size (fun i -> [(ind, Predicate.PInt i)])) in
					expand_template (i+1) arrs arrcells res
				else res in
			(* We expand the template because we need to test every possible solution for arrcell *)	
			let template = Predicate.big_and (
				let (arrs, arrcells) = Hashtbl.fold (fun arr arrcell (resarr, rescell) -> 
					(resarr@[arr], rescell@[arrcell])
				) arrcells ([], []) in
				let subs = expand_template 0 arrs arrcells [] in
				let _ = assert (List.length subs <> 0) in
				List.map (fun sub -> 
					(**Every possible soltuion for arrcell may need to match to a different excell*)
					let exsub = Hashtbl.fold (fun arr (excell, _, _) res -> 
						res @ [(excell, Predicate.Var (Path.mk_ident (Path.name excell)))]
					) arrexcells [] in
					let pred = Predicate.apply_substs sub template in
					Predicate.apply_substs exsub pred
				) subs
			) in
			(* In addition to do instantiation we also regulate the real array values;
			  Also each array variable is replaced by a different instantiation *)
			(*let cf = Predicate.big_and (template::(List.map (fun bound -> Predicate.Atom (Predicate.PInt 0,Predicate.Le, bound)) bounds)) in*)
			let cf = Predicate.big_and (template::(Hashtbl.fold (fun arr arrbound res -> 
				if (user_assertion_flag || ex_assertion_flag) then 
					let _ = assert (Hashtbl.mem arrlowerbounds arr) in
					let arrlowerbound = Hashtbl.find arrlowerbounds arr in
					let c1 = Predicate.Atom (Predicate.PInt 0, Predicate.Le, arrlowerbound) in (*0 <= lowerbound*)
					let c2 = Predicate.Atom (arrbound, Predicate.Le, Predicate.FunApp ("Array.length", [Predicate.Var arr])) in (*higherbound <= array.length*)
					let c3 = Predicate.Atom (Predicate.Binop (arrlowerbound, Predicate.Minus, Predicate.PInt (1)), 
																	Predicate.Le, arrbound) in (*lowerbound <= higherbound*)
					(* lowerbound <> higherbound in terms of parametric part *)		
					let lower_coeffs = get_coeffs arrlowerbound in
					let higher_coeffs = get_coeffs arrbound in
					(* remove the coeff for len a *)
					let higher_coeffs = if (ex_assertion_flag) then List.tl higher_coeffs else higher_coeffs in
					let _ = assert (List.length lower_coeffs = List.length higher_coeffs) in										
					let c4 = Predicate.Not (Predicate.big_and (List.map2 (fun co1 co2 -> 
						Predicate.Atom (co1, Predicate.Eq, co2)) lower_coeffs higher_coeffs)) in
					let c = Predicate.big_and [c1;c2;c3;c4] in
					res @ [c]
				else
					let c1 = Predicate.Atom (Predicate.PInt 0, Predicate.Le, arrbound) in
					let c2 = Predicate.Atom (arrbound, Predicate.Le, Predicate.FunApp ("Array.length", [Predicate.Var arr])) in
					let c = Predicate.And (c1, c2) in
					res @ [c]	
			) arrbounds [])) in
			
			(**********************************************************************************************************)
			(** The bound for excell should also be exerted *)
			let cf = Predicate.big_and (cf::(Hashtbl.fold (fun arr (_, exlowerbound, exhigherbound) res-> 
				let c1 = Predicate.Atom (Predicate.PInt 0, Predicate.Le, exlowerbound) in
				let c2 = Predicate.Atom (exhigherbound, Predicate.Le, Predicate.FunApp ("Array.length", [Predicate.Var arr])) in
				let c3 = Predicate.Atom ( (exlowerbound, Predicate.Le, exhigherbound)) in
				(* lowerbound <> higherbound in terms of parametric part *)		
				let lower_coeffs = get_coeffs exlowerbound in
				let higher_coeffs = get_coeffs exhigherbound in	
				let higher_coeffs = if (ex_assertion_flag) then List.tl higher_coeffs else higher_coeffs in			
				let _ = assert (List.length lower_coeffs = List.length higher_coeffs) in					
				let c4 = Predicate.Not (Predicate.big_and (List.map2 (fun co1 co2 -> 
					Predicate.Atom (co1, Predicate.Eq, co2)) lower_coeffs higher_coeffs)) in
				let c = Predicate.big_and [c1;c2;c3;c4] in
				res @ [c]
			) arrexcells [])) in
			
			let cf = Predicate.big_and ((Predicate.apply_substs subs cf)::arrcons) in
			let subs = List.map (fun arr -> (arr, Predicate.Var (Path.mk_ident (Path.name arr)))) arrs in
			(** len_a should also be distinguished among different samples *)
			(*let subs = subs @
				List.map (fun (arr, arr') -> 
					let arr' = Predicate.exp_var arr' in
					(Path.Pident (Ident.create_with_stamp ("len_"^(Path.name arr)) (Path.stamp arr)), 
					Predicate.Var (Path.Pident (Ident.create_with_stamp ("len_"^(Path.name arr)) (Path.stamp arr')))
				)) subs in*)
			(** oldarray name should also be distinguished among different samples *)
			let subs = subs @ 
				(List.map (fun arr -> 
					let v = Path.mk_ident (Frame.old_array_flag ^ (Path.name arr)) in
					(v, Predicate.Var v)) arrs) in
			let cf = Predicate.apply_substs subs cf in
			Predicate.map_expr (fun pexpr -> match pexpr with
				| Predicate.FunApp (name, args) -> 
					(try 
						let (_,p') = List.find (fun (p, p') -> String.compare name (Path.name p) = 0) subs in
						match p' with
							| Predicate.Var p' -> Predicate.FunApp (Path.unique_name p', args)
							| _ -> assert false
					with _ -> pexpr)
				| _ -> pexpr) cf
		) samples in
		(* A solution to the template should fit all samples *)
		let cf = Predicate.big_and predicates in
		(* But we want to further limit coeffs can only be assigned to -1, 0, 1 *)
		let coeff_ranges = Predicate.big_and (Hashtbl.fold (fun k _ res -> 
			Predicate.And (
				Predicate.Atom (Predicate.PInt (0-1),Predicate.Le,k), 
				Predicate.Atom (k,Predicate.Le,Predicate.PInt 1))::res) coeffs []) in
		(* And we want to furhter limit some coeffs cannot be 0 *)
		let coeff_constraints = Predicate.big_and (List.map (fun coeffc -> 
			let _ = assert (List.length coeffc != 0) in
			Predicate.big_or (List.map (fun coeff -> Predicate.Atom (coeff,Predicate.Ne,Predicate.PInt 0)) coeffc)	
		) (!coeff_constraints)) in
		(* And we want to further limit the occurance of non-zeor coeffs in user-defined assertions *)
		let _ = if (user_assertion_flag || ex_assertion_flag) then assert (List.length (!coeff_constraints2) > 0) in
		let _ = if (List.length (!coeff_constraints2) > 0) then assert (user_assertion_flag || ex_assertion_flag) in
		let user_defined_constraints = Predicate.big_and (List.map (fun coeffc ->
			let _ = assert (List.length coeffc <> 0) in
			let atmost1 = List.map (fun c -> 
				let o = List.fold_left (fun res c' -> 
					if (c = c') then res
					else
						res @ [Predicate.Atom (c', Predicate.Eq, Predicate.PInt 0)]) [] coeffc in
				if (List.length o > 0) then 
					let o = Predicate.big_and o in
					Predicate.And (o, Predicate.Atom (c,Predicate.Ne,Predicate.PInt 0))
				else Predicate.Atom (c,Predicate.Ne,Predicate.PInt 0)
			) coeffc in
		 	let all0 = Predicate.big_and (List.map (fun c -> 
				Predicate.Atom (c,Predicate.Eq,Predicate.PInt 0)) coeffc) in
			Predicate.big_or (all0::atmost1)
		) (!coeff_constraints2)) in
		(*let _ = if (user_assertion_flag) then (Format.fprintf Format.std_formatter 
							"user_defined_constraints:%a@." Predicate.pprint' user_defined_constraints; assert false) in*)
		(* And we want to bound the constants that appears in the template *)
		let d_ranges = Predicate.big_and (Hashtbl.fold (fun d _ res -> 
			Predicate.And (
				Predicate.Atom (Predicate.PInt (0-1),Predicate.Le,d),
				Predicate.Atom (d,Predicate.Le,Predicate.PInt 1))::res) ds []) in
		(* The constraint formula that must be met by the samples *)		
		let cf = 
			if (user_assertion_flag || ex_assertion_flag) then
				Predicate.big_and [d_ranges; coeff_ranges; coeff_constraints; user_defined_constraints; cf] 
			else Predicate.big_and [d_ranges; coeff_ranges; coeff_constraints; cf] in
		let _ = Format.fprintf Format.std_formatter "cf=%a@." Predicate.pprint' cf in
		let solutions = TheoremProver.model cf 1 in
		if (List.length solutions = 0) then []
		else 
			let solution = List.hd solutions in
			let coeffs = Hashtbl.fold (fun k _ res -> match k with Predicate.Var k -> k::res | _ -> assert false) coeffs [] in
			let ds = Hashtbl.fold (fun d _ res -> match d with Predicate.Var d -> d::res | _ -> assert false) ds [] in
			let solutions = allsolution [solution] cf (coeffs@ds) in
			List.map (fun solution -> 
				(*let _ = List.iter (fun cvar -> 
					let value = try Hashtbl.find solution cvar with _ -> assert false in
					Format.fprintf Format.std_formatter "%s: %d " (Path.unique_name cvar) value
				) coeffs in
				let _ = Format.fprintf Format.std_formatter "@." in*)
				let solution = Hashtbl.fold (fun p i res -> res@[(p, Predicate.PInt i)]) solution [] in
				let pred = Predicate.apply_substs solution simple_template in 
				(*let _ = Format.fprintf Format.std_formatter "inv: %a@." Predicate.pprint pred in *)
				pred
			) solutions
	) templates))
	(************* End to work with each template ******************)	
	(************* End to work with each template ******************)	
	(************* End to work with each template ******************)	
