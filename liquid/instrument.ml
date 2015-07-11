(** Insert sampling statements into the analyzing code *)

open Parsetree
open Typedtree
open Types
open Frame
open Longident
open Backwalker

let pp = Format.fprintf

let log_fname = "mllog"

let tempt_arr_prefix = "tmp_pre"
let tempt_arr_postfix = "temp_post"

(*If a function is a measure => do not instrument it*)
let is_measure se_env funname = 
	(String.compare funname "List.length" = 0) || 
	(Hashtbl.fold (fun _ ms res -> 
		if (res) then res
		else
			List.exists (fun (m, _) -> String.compare funname (Path.name m) = 0) ms
	) se_env.measures false)
	
(*If a function is userdefined => may instrument it*)
let is_user_function se_env funname = 
	Hashtbl.fold (fun f _ res ->
		if (res) then res
		else String.compare funname (Path.name f) = 0
	) se_env.funframebindings false

(** expression is from text; count how many arguments are specified *)
let rec count_arguments expression = match expression.pexp_desc with
	| Parsetree.Pexp_function (lbl, elbl, exp) -> 
		let (p, e) = List.hd exp in
		if lbl="" then
    	match e.pexp_desc with
      | Pexp_when _ -> 0
			| _ -> 1 + count_arguments e
		else 1 + count_arguments e
	| _ -> 0

let count_app_arguments expression = 
	let rec fold_string_list res fs = match fs with
	| [] -> res
	| [a] -> res ^ a
	| a::l -> fold_string_list (res ^ a ^ ".") l in
	match expression.pexp_desc with
	| Parsetree.Pexp_apply (e, l) -> (match e.pexp_desc with
		| Parsetree.Pexp_ident id -> 
			let names = Longident.flatten id in
			(fold_string_list "" names, List.length l)
		| _ -> assert false)
	| _ -> assert false

let is_base_typed f = match f with
	| Frame.Farrow _ -> false
	| Frame.Frecord _ -> false
	| Frame.Ftuple _ -> false
	| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list -> false
	| f -> true

(** Print out min and max functions for boundary of higher order functions *)
let print_min_max formatter = 
	(pp formatter 
	"@;@[<hov2>let@ min l =@ List.fold_left (fun res li -> min res li) (-1) l @]@.";
	pp formatter 
	"@;@[<hov2>let@ max l =@ List.fold_left (fun res li -> max res li) 0 l @]@.")
	
(** Print out test harnesses if programmers specify them in a separate file *)
let print_harnesses fname formatter = 
	let fname = fname ^ "_harness" in
	try 
		let lines = ref [] in
		let chan = open_in fname in
		let _ = try
  		while true; do
    		lines := input_line chan :: !lines
  		done
		with End_of_file ->
  		close_in chan in
  	let lines = List.rev !lines in
		List.iter (fun line -> 
			pp formatter "@;@[<hov2>%s@;@]" line;
		) lines
	with _ -> () (* No test harnesses available in a seperate file. Fine. *)	
	
let is_recursive_type path dec = 
	let allparams = match dec.type_kind with
		| Type_variant decs -> decs
		| kind -> assert false in
	List.exists (fun (cstrname, params) -> 
		List.exists (fun param -> 
			match param.desc with (*Must FIXME.*)
				| Tconstr (p, _, _) when (Path.same p path) -> true
				| Tconstr (p, _, _) when (Path.same p Predef.path_list) -> true
				| _ -> false
			) params 
	) allparams	
	
(** Dump inductive data structures ...  *)	
let print_udt types udt_table ppf = 
	Hashtbl.iter (fun path declaration -> 
		if List.exists (fun ty -> 
			String.compare ty (Path.name path) = 0) types &&
			is_recursive_type path declaration then 
		let allparams = match declaration.type_kind with 
			| Type_variant decs -> decs
			| kind -> assert false in	
		(* Type of allparams : (string * type_expr list) list *)
		(** printf constructor argument *)
		let log_cstr_params n = 
			let _ = if (n > 0) then (pp ppf "(") in
			let args = Array.init n (fun i -> "t_"^(string_of_int i)) in
			let _ = Array.iteri (fun i arg -> 
				if (i < n - 1) then pp ppf "%s, " arg
				else pp ppf "%s" arg
				) args in
			if (n > 0) then pp ppf ")" in
		(** printf the process of this constructor *)		
		let log_cstr_process cstrname links values = 
			if (links = []) then 
				(if (values = []) then pp ppf "@; @[None@]"
				else 	
					let value = List.hd values in
					(pp ppf "@; @[(fprintf outch (\"%s#%d#%s;%s,\") t_%d (-1000); @]" cstrname (-1) "%d" "%d" value;
					pp ppf "@; @[Some t_%d)@]" value))
			else
				(List.iter (fun (link, p) -> 
					if (Path.same p Predef.path_list) then
						(pp ppf "@; @[let ele_%d = @;@[%a@] in @]"
							link
							(fun ppf unit -> 
								pp ppf "@[List.fold_right (fun v res -> @;@[%a@] @;@[%a@]) t_%d (-1000) @]"
									(fun ppf unit -> 
										pp ppf "@; @[let ele = sample_%s v in @]" (Path.name path)
										) ()
									(fun ppf unit ->
										pp ppf "@; @[match ele with @. | None -> res @. | Some ele -> (if (!callflag) then fprintf outch (\"Cons#1#%s;%s,\") ele res; ele) @]" "%d" "%d"
										) ()
									link
								) ();
							pp ppf "@; @[let ele_%d = if ele_%d = (-1000) then None else Some ele_%d in @]" link link link
							)
					else pp ppf "@; @[let ele_%d = sample_%s t_%d in @]" link (Path.name path) link
					) links;	
				if (values = [] && links <> []) then (
					List.iter (fun (link, _) -> 
						pp ppf "@; @[let _ = match ele_%d with @. | None -> if (!callflag) then fprintf outch (\"%s#%d#%s;%s,\") %s (-1000) @. | Some ele_%d -> @. if (!callflag) then fprintf outch (\"%s#%d#%s;%s,\") %s ele_%d in @]" 
													link
													(cstrname) (link) "%d" "%d" "(!xxxcounter)"
													link (cstrname) (link) "%d" "%d" "(!xxxcounter)" link 
						) links;
						pp ppf "@; @[let c = (!xxxcounter) in (xxxcounter := c+1; Some c)@]"
				)
				else (
				List.iter (fun value -> 
					List.iter (fun (link, _) -> 
						pp ppf "@; @[let _ = match ele_%d with @. | None -> if (!callflag) then fprintf outch (\"%s#%d#%s;%s,\") t_%d (-1000) @. | Some ele_%d -> @. if (!callflag) then fprintf outch (\"%s#%d#%s;%s,\") t_%d ele_%d in @]" 
													link
													(cstrname) (link) "%d" "%d" value
													link (cstrname) (link) "%d" "%d" value link 
						) links
					) values;
				if (values = []) then pp ppf "@[None@]"
				else pp ppf "@; @[(Some t_%d)@]" (List.hd values)) ) in
		(** printf the whole sampling strategy for user inductive data type *)
		pp ppf "@;@[%a %a @]"   
		(fun ppf unit -> pp ppf "@[<hov2>let rec sample_%s t = @. match t with @]" (Path.name path)) ()
		(fun ppf unit -> (*pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%a@] @;@[%a@]; @;@[%s@];)@]@]" *)
			List.iter (fun (cstrname, params) -> 
				let (links, values, _) = List.fold_left (fun (links, values, index) param -> 
					match param.desc with (*Must FIXME.*)
						| Tconstr (p, _, _) when (Path.same p path) -> (links @ [(index, p)], values, index+1)
						| Tconstr (p, tys, _) when (Path.same p Predef.path_list) -> 
							(let _ = assert ((List.length tys) = 1) in
							let ty = List.hd tys in
							match ty.desc with
								| Tconstr _ -> (links @ [(index, p)], values, index+1)
								| _ -> (links, values, index+1))
						| Tconstr _ -> (links, values, index+1)
						| _ -> (links, values @ [index], index+1)
					) ([], [], 0) params in
				let n = List.length params in
				pp ppf "@; @[| %s %a -> @. %a  @]" 
					cstrname 
					(fun ppf unit -> ignore (log_cstr_params n)) () 
					(fun ppf unit -> ignore (log_cstr_process cstrname links values)) ()
				) allparams	) ()
	) udt_table
	
(** funpexpr is of f x y. freevars include x or y if any of them free.
  * for each free var, return its upper and lower bound
	*)
let random_value freevars pred = 
	let lowers = (Hashtbl.create 5) in
	let uppers = (Hashtbl.create 5) in
	let _ = List.iter (fun freevar -> 
		Hashtbl.replace lowers freevar [];
		Hashtbl.replace uppers freevar []
		) freevars in
	let _ = Predicate.map_pred (fun pred -> match pred with
		| Predicate.Atom (Predicate.Var var, Predicate.Gt, bound) ->
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace lowers var (bound::(Hashtbl.find lowers var));
			pred)
		| Predicate.Atom (Predicate.Var var, Predicate.Ge, bound) -> 	
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace lowers var (bound::(Hashtbl.find lowers var));
			pred)
		| Predicate.Atom (bound, Predicate.Gt, Predicate.Var var) -> 	
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace uppers var (bound::(Hashtbl.find uppers var));
			pred)
		| Predicate.Atom (bound, Predicate.Ge, Predicate.Var var) -> 	
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace uppers var (bound::(Hashtbl.find uppers var));
			pred)
		| Predicate.Atom (Predicate.Var var, Predicate.Lt, bound) ->
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace uppers var (bound::(Hashtbl.find uppers var));
			pred)
		| Predicate.Atom (Predicate.Var var, Predicate.Le, bound) -> 	
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace uppers var (bound::(Hashtbl.find uppers var));
			pred)
		| Predicate.Atom (bound, Predicate.Lt, Predicate.Var var) -> 	
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace lowers var (bound::(Hashtbl.find lowers var));
			pred)
		| Predicate.Atom (bound, Predicate.Le, Predicate.Var var) -> 
			(if (List.exists (fun freevar -> Path.same freevar var) freevars) then
				Hashtbl.replace lowers var (bound::(Hashtbl.find lowers var));
			pred)	
		| _ -> pred
		) pred in
	(lowers, uppers)
	
let rec delete_redundant_UF es = 
	(*begin:delete redundant UFs*)
	match es with
		| (Predicate.FunApp ("UF", es')) :: es -> 
			let es = delete_redundant_UF es in 
			let (f, args) = (List.hd es', List.tl es') in
			(match f with
				| Predicate.Var f -> (Predicate.FunApp (Path.name f, args)) :: es
				| _ -> assert false)
		| e::es -> e::(delete_redundant_UF es) 
		| [] -> [] (*end*)	

(** Give a test to higher order function *)
let make_test localdefs fr = 
	(** frees --> the input parameters *)
  (** args --> the synthesized arguments for the test call *)
	let localdefs = try List.remove_assoc (Frame.returnpath) localdefs 
									with _ -> assert false in
	let rec frame_equals f1 f2 = match (f1, f2) with
		| (Frame.Farrow (_, f1, f2, _), Frame.Farrow (_, f1', f2', _)) ->
			(frame_equals f1 f1' && frame_equals f2 f2')
		| (Frame.Fconstr (p,fs,_,_,_), Frame.Fconstr (p',fs',_,_,_)) -> 
			(Path.same p p') && (List.length fs == List.length fs') && 
			(List.for_all2 (fun f f' -> frame_equals f f') fs fs') 
		| (Frame.Frecord _, Frame.Frecord _) -> true
		| (Frame.Ftuple _, Frame.Ftuple _) -> true
		| (Frame.Fvar _, Frame.Fvar _) -> true
		| _ -> false in (* Fixme. The above implementation must be fixed! *)
	let rec loop flag frees args rfrs fr = match fr with
		| Frame.Farrow (_, f1, f2, _) -> 
			if (flag) then
				let (frees, args, rfrs) = loop false frees args rfrs f1 in
				(match f2 with
					| Frame.Farrow _ -> loop true frees args rfrs f2
					| fr -> (frees, args, [fr]))
			else 
				let (v, _) = 
					try List.find (fun (_, ld) -> frame_equals ld fr) localdefs 
					with _ -> (assert false) in
				(frees, args@[v], rfrs)
		| Frame.Fvar _ ->
			let v = Path.mk_ident ("v" ^ string_of_int (List.length frees)) in
			(frees@[v], args@[v], rfrs)
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_int -> 
			let v = Path.mk_ident ("v" ^ string_of_int (List.length frees)) in
			(frees@[v], args@[v], rfrs)
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_unit ->
			(frees, args@[Path.mk_ident "unit"], rfrs)
		| Frame.Ftuple (fs, _) ->
			List.fold_left (fun (frees, args, rfrs) fr -> 
				loop flag frees args rfrs fr	
			) (frees, args, rfrs) fs
		| Frame.Frecord (_,fs,_) -> 
			List.fold_left (fun (frees, args, rfrs) (fr,_,_) -> 
				loop flag frees args rfrs fr
			) (frees, args, rfrs) fs
		| _ -> 
			let (v, _) = List.find (fun (_, ld) -> frame_equals ld fr) localdefs in
			(frees, args@[v], rfrs) in
	let (frees, args, rfrs) = loop true [] [] [] fr in
	(frees, List.map (fun arg -> Predicate.Var arg) args, List.hd rfrs)

(** Synthesize the call to a higher order function *)
let synthesize formatter funname bad_constraint locals defs allbindings =	
	(** Search symbolic constraint *)
	let fun_stack = ref [] in	
	let _ = Predicate.map_expr (fun pexpr -> match pexpr with
		| Predicate.FunApp (fn, es) when (String.compare fn "UF" = 0) ->  
			let funpath = List.hd es in
			(match funpath with
				| Predicate.Var funpath -> 
					if (String.compare funname (Path.name funpath) = 0) then
						try
							let (_,fr) = List.find (fun (p,f) -> Path.same p funpath) allbindings in
							let n_args = Frame.count_args fr in
							let n_actuals = (List.length es)-1 in
							if (n_actuals >= n_args) then
								(fun_stack := pexpr::(!fun_stack); pexpr)
							else pexpr
						with _ -> assert false
					else pexpr
				| _ -> (assert false))
		| _ -> pexpr
		) bad_constraint.post in (** search all higher order function constriants *)
	(** defines how to print higher order function arguments *)
	let log_args formatter args =  
		List.fold_left (fun index arg -> match arg with
			| Predicate.FunApp (fn, es) when (String.compare fn "UF" = 0) -> assert false
				(*let funpath = List.hd es in
				(match funpath with
					| Predicate.Var funpath ->
						(try 
							let (_, fr) = List.find (fun (p,f) -> Path.same p funpath) (allbindings) in 
							let n_args = Frame.count_args fr in
							let n_actuals = (List.length es)-1 in
							if (n_actuals >= n_args) then 
								let _ = pp formatter
								"@[(try fprintf outch (\"%s#%s,\") ((%a)) with _->()); @]"
								(funname ^ "_" ^ (string_of_int index)) "%d" Predicate.pprint_pexpr arg in
								(index+1)
							else (index+1)
						with _ -> assert false)
					| _ -> (Format.fprintf Format.std_formatter "Fail to deal with %a@." Predicate.pprint_pexpr arg; assert false))*)
			| Predicate.FunApp (fn, es) when (String.compare fn "List.hd" = 0  || String.compare fn "List.tl" = 0) 
				(* The argument should be a int list only => ignore any int int... int list *) ->
				let vars = Predicate.exp_vars arg in
				let b = List.for_all (fun var -> 
					try 
						let (_, fr) = List.find (fun (p,f) -> Path.same p var) (allbindings) in
						match fr with 
							| Frame.Fconstr (x,fs,_,_,_) when x = Predef.path_list -> is_base_typed (List.hd fs)								 
							| fr -> assert false
					with _ -> assert false 
				) vars in
				if (b) then
					let _ = pp formatter
					"@[(try fprintf outch (\"%s#%s,\") ((%a)) with _->()); @]"
					(funname ^ "_" ^ (string_of_int index)) "%d" Predicate.pprint_pexpr arg in
					(index+1)
				else (index + 1)
			| Predicate.FunApp (fn, es) when (String.compare fn "List.length" = 0) ->  
				let _ = pp formatter
				"@[(try fprintf outch (\"%s#%s,\") ((%a)) with _->()); @]"
				(funname ^ "_" ^ (string_of_int index)) "%d" Predicate.pprint_pexpr arg in
				(index+1)
			| Predicate.FunApp (funpath, es) -> 
				(*(Format.fprintf Format.std_formatter "Ill arg: %a@." Predicate.pprint_pexpr arg; assert false)*)
				((try 
					let (_, fr) = List.find (fun (p,f) -> String.compare (Path.name p) funpath = 0) (allbindings) in 
					let n_args = Frame.count_args fr in
					let n_actuals = (List.length es) in
					if (n_actuals >= n_args) then 
						let _ = pp formatter
						"@[(try fprintf outch (\"%s#%s,\") ((%a)) with _->()); @]"
						(funname ^ "_" ^ (string_of_int index)) "%d" Predicate.pprint_pexpr arg in
						(index+1)
					else (index+1)
				with _ -> assert false))
			| _ ->
				let vars = Predicate.exp_vars arg in
				let b = List.for_all (fun var -> 
					try 
						(let (_, fr) = List.find (fun (p,f) -> Path.same p var) (allbindings) in 
						match fr with
							| Frame.Fconstr (x,_,_,_,_) when (x = Predef.path_unit) -> false
							| Frame.Farrow _ -> false
							| _ -> true)
					with _ -> (* Fixme. Hard coded. *)
							(if (String.compare (Path.name var) "unit" = 0) then false
							else true)
				) vars in
				if (b) then
					let _ = pp formatter
					"@[fprintf outch (\"%s#%s,\") ((%a)); @]"
					(funname ^ "_" ^ (string_of_int index)) "%d" Predicate.pprint_pexpr arg in
					(index+1)
				else (* this argument is not an integer so just forget about it *) (index+1)
		) 0 args in
			
	let print_callargs formatter args =
		List.iter (fun arg -> match arg with (* Fixme. Hard coded. *)
			| Predicate.Var p when (String.compare (Path.name p) "unit" = 0) -> 
				pp formatter " %s" "()"
			| _ -> 
				pp formatter " %a" Predicate.pprint_pexpr arg
			) args in
			
	let fun_stack = Common.remove_duplicates !fun_stack in
	(*let _ = Format.fprintf Format.std_formatter "---- \n" in
	let _ = List.iter (fun f -> 
		(Format.fprintf Format.std_formatter "%a is in funstack! \n" Predicate.pprint_pexpr f)
		) fun_stack in
	let _ = List.iter (fun (p, _) -> Format.fprintf Format.std_formatter "def %s@." (Path.unique_name p)) (defs) in
	let _ = List.iter (fun (p, _) -> Format.fprintf Format.std_formatter "local %s@." (Path.unique_name p)) (locals) in	
	let _ = Format.fprintf Format.std_formatter "---- \n" in*)
	if (List.length fun_stack > 0) then
		List.iter (fun fun_pexpr -> match fun_pexpr with
			| Predicate.FunApp ("UF", _::callargs) -> 
				let callargs = delete_redundant_UF callargs in
				let frees = List.fold_left (fun freeset callarg ->
					(** callarg may contain some variable not in scope; generate int for them *)
					let callargvars = Predicate.exp_vars callarg in
					(** arguments should be bound by parameters 
							and environment that the function is defined *)
					let frees = List.filter (fun callargvar -> 
						List.for_all (fun (def,_) -> not (Path.same def callargvar)) (locals@defs) &&
						(* In the meantime free must be an integer. *)
						(* Question: how about we have a free that is a list? Fixme!! *)
						(try 
							let (_, fr) = List.find (fun (p, _) -> Path.same callargvar p) allbindings in 
							(match fr with 
								| Frame.Fconstr (p,_,_,_,_) when (Path.same p Predef.path_int) -> true 
								| Frame.Fvar _ -> true | _ -> false) 
						with _ -> true)
						) callargvars in
					if (List.length frees = 0) then freeset
					else (
						(*Printf.fprintf stdout "funname = %s \n" funname;
						List.iter (fun free -> Printf.fprintf stdout
						"free var %s \n" (Path.name free)) frees;
						List.iter (fun (def,_) -> Printf.fprintf stdout
						"def var %s \n" (Path.name def)) (locals@defs);*)
						freeset @ frees)
					) [] callargs in 
				(** call higher order function *)
				if (List.length frees > 0) then
					let (lowers, uppers) = random_value frees bad_constraint.post in
					pp formatter "@;@[%a %a %a@]"
					(fun ppf unit ->
						(List.iter (fun free -> 
							(** the upper and lower bounds of free bounds a loop *)
							let uppers = Common.remove_duplicates (Hashtbl.find uppers free) in
							let lowers = Common.remove_duplicates (Hashtbl.find lowers free) in			
							(* Forbid any bound that is same to fun_pexpr. Fix me? Reject all function call based bound? *)
							let uppers = List.filter (fun upper -> not (upper = fun_pexpr)) uppers in
							let lowers = List.filter (fun lower -> not (lower = fun_pexpr)) lowers in	
							(*let _ = List.iter (fun upper -> Format.fprintf Format.std_formatter "upper=%a@." Predicate.pprint_pexpr upper) uppers in
							let _ = List.iter (fun lower -> Format.fprintf Format.std_formatter "lower=%a@." Predicate.pprint_pexpr lower) lowers in*)		
							let int_locals = Common.map_partial (fun (v, f) -> match f with
								| Frame.Fvar _ -> Some v
								| Frame.Fconstr (p,_,_,_,_) when Path.same p Predef.path_int -> Some v
								| _ -> None
								) locals in
							let printbounds flag formatter bounds = 
								if (List.length bounds = 0) then 
									(** if free is actually a parameter then use it *)
									if (List.exists (fun (p, _) -> Path.same p free) allbindings && 
										not (List.exists (fun p -> Path.same p free) int_locals)) then
										Common.pprint_list "; " (fun formatter path -> 
											pp formatter "%s" (Path.name path)
											) formatter (free::int_locals)
									else
										Common.pprint_list "; " (fun formatter path -> 
											pp formatter "%s" (Path.name path)
											) formatter int_locals
								else 
									(** filter bounds that are not going out of scope *)
									let printbound formatter bound = 
										(*let _ = Format.fprintf Format.std_formatter "A bound as %a@." Predicate.pprint_pexpr bound in*)
										let boundvars = Predicate.exp_vars bound in 
										let unboundvars = List.filter (fun boundvar ->
											List.for_all (fun (def,_) -> not (Path.same def boundvar)) (locals @ defs)
											) boundvars in
										if (List.length unboundvars = 0) then
											Predicate.pprint_pexpr formatter bound
										else 
											(* Although this bound is intended to be eliminated, but it may actually refer to a function paprameter *)
											(*let b = List.for_all (fun uv -> List.exists (fun (p, _) -> Path.same uv p) allbindings) unboundvars in*)
											let fn = if flag then "max" else "min" in
											let args = List.fold_left (fun res arg -> res^(Path.name arg)^";") "" int_locals in
											let unbound_substs = List.map (fun ubv -> 
												(ubv, Predicate.Var (Path.Pident (Ident.create_persistent (fn ^ "["^ args ^"]")))
												)) unboundvars in
											Predicate.pprint_pexpr formatter (
											Predicate.exp_apply_substs unbound_substs bound) in
									Common.pprint_list "; " printbound formatter bounds in
							pp ppf "@[in let _ = for %s = min([%a]) to max([%a]) do@]" 
							(Path.name free) (printbounds false) lowers (printbounds true) uppers
							) frees)) ()
							(*"@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]"*)
						(fun ppf unit -> pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%a@] @;@[%a@]; @;@[%s@];)@]@]" 
							("(callflag := false)")
							(fun ppf unit -> pp ppf "@[fprintf outch (\"%s:\")@]" funname) ()
							(fun ppf unit -> ignore(log_args ppf callargs)) ()
							(fun ppf unit -> pp ppf "@[(try fprintf outch (\"%s#%s\\t\") ((%s%a)) with _->(fprintf outch (\"\t\")))@]"
							(funname ^ "_" ^ "r") "%d" funname (print_callargs) callargs;) ()
							("(callflag := true)")) ()
						(fun ppf unit -> 
							List.iter (fun _ -> pp ppf "@[%s@]" "done") frees
							) ()
				else 
					let _ = pp formatter "@;@[in let _ = if (!callflag) then ((callflag := false); @]" in
					let _ = pp formatter "@;@[fprintf outch (\"%s:\"); @]" funname in 
					let _ = log_args formatter callargs in (
					pp formatter 
					"@;@[(try fprintf outch (\"%s#%s\\t\") ((%s%a)) with _->(fprintf outch (\"\t\"))); @]"
					(funname ^ "_" ^ "r") "%d" funname (print_callargs) callargs;
					pp formatter 
					"@;@[(callflag := true)) @]")
			| _ -> assert false
			) fun_stack
	else (** No constraints from post-condition; try to directly call it *)	
		let (_, fr) = List.find (fun (p,f) -> String.compare (Path.name p) funname = 0) (allbindings) in 
		(**--------- check each argument to make sure it actually can be called -----------------*)
    try let (frees, callargs, rfr) = make_test (allbindings@defs) fr in
		pp formatter "@;@[in let _ = %a %a %a@]"
		(fun ppf unit ->
			(List.iter (fun free -> 	
				let int_locals = Common.map_partial (fun (v, f) -> match f with
					| Frame.Fvar _ -> Some v
					| Frame.Fconstr (p,_,_,_,_) when Path.same p Predef.path_int -> Some v
					| _ -> None
					) (locals@defs) in
				let printbounds flag formatter bounds = 
					(*Fixme. Intergers -1, 1 should be min and max int used in the program*)
					if (List.length bounds = 0) then
						if (flag) then pp formatter "1"
						else pp formatter "-1"
					else
						if (flag) then
							Common.pprint_list "; " (fun formatter path -> 
								pp formatter "%s+1" (Path.name path)
								) formatter bounds 
						else 
							Common.pprint_list "; " (fun formatter path -> 
							pp formatter "%s-1" (Path.name path)
							) formatter bounds in
				pp ppf "@[for %s = min([%a]) to max([%a]) do @]" 
				(Path.name free) (printbounds false) int_locals (printbounds true) int_locals
				) frees)) ()
				(*"@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]"*)
			(fun ppf unit -> pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%a@] @;@[%a@]; @;@[%s@];)@]@]" 
				("(callflag := false)")
				(fun ppf unit -> pp ppf "@[fprintf outch (\"%s:\")@]" funname) ()
				(fun ppf unit -> ignore(log_args ppf callargs)) ()
				(fun ppf unit -> (* If the function return is not an integer type, then just print 0 *)
					match rfr with
						| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_int -> 
							pp ppf "@[(try fprintf outch (\"%s#%s\\t\") ((%s%a)) with _->(fprintf outch (\"\t\")))@]"
							(funname ^ "_" ^ "r") "%d" funname (print_callargs) callargs
						| frame -> 
							pp ppf "@[(try fprintf outch (\"%s#%s\\t\") ((%s%a); 0) with _->(fprintf outch (\"\t\")))@]"
							(funname ^ "_" ^ "r") "%d" funname (print_callargs) callargs) ()
				("(callflag := true)")) ()
			(fun ppf unit -> 
				List.iter (fun _ -> pp ppf "@[%s@]" "done ") frees
				) () with _ -> ()
		(**--------------------------------------------------------------------------------------*)
		
let is_base_typed f = match f with
	| Frame.Farrow _ -> false
	| Frame.Frecord _ -> false
	| Frame.Ftuple _ -> false
	(*| Frame.Fvar _ -> false*)
	| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_list -> false
	| f -> true

let decode_record name n = 
	let names = Str.split (Str.regexp "\\.") name in	
	(*let _ = Format.fprintf Format.std_formatter "record_name = %s@." name in
	let _ = List.iter (fun name -> Format.fprintf Format.std_formatter "split_record_name=%s@." name) names in*)
	if (List.length names < 2) then name
	else
		let (name, fields) = (List.hd names, List.tl names) in
		List.fold_left (fun res field -> 
			try 
				let proj = int_of_string field in
				let is = Array.to_list (Array.init n (fun i -> i)) in
				let arg = List.fold_left (fun res i -> 
					let res = if (i = proj) then res ^ "x" else res ^ "_" in
					if (i = n - 1) then res else res ^ ","
				) "" is in
				"((fun (" ^ arg ^") -> x) (" ^ res ^ "))"
			with _ -> res ^ "." ^ field
		) name fields
		
(** dump any interesting parameter or return construct *)			
let rec dump formatter env fenv measures pat frame bad_constraint locals defs allbindings n_args =
	let patfrs = bind env pat frame in
	(if n_args > 0 then List.iter (fun (path, fr) -> 
		let pathname = Path.ident_name path in 
		match pathname with
			| Some x -> (
				(** Ocaml does not support tuple selection. n_args is used for the len of the tuple. So x.0 --> (fun (x,_) -> x) x *)
				let xv = decode_record x n_args in
				match fr with
				| Fconstr (y,_,_,_,_) when y = Predef.path_unit -> ()
				| Fconstr (y,fs,_,_,_) when y = Predef.path_list && is_base_typed (List.hd fs) -> (* printing list *)
					if !(Clflags.reachability) then
						(** Print linkability; Should consider if the content of the list is another container *)
						match (List.hd fs) with
						| Fconstr (y,_,_,_,_) when Hashtbl.mem measures y -> 
							pp formatter "@;@[%a %a %a %a@]"
							(fun ppf unit -> pp ppf "@[in let _ = fprintf outch (\"%s:\");@]" (x^"_heap")) () 
							(fun ppf unit -> pp ppf "@[ignore (List.fold_right (fun v res -> @;@[%a@] @;@[%a@]) %s (-1000)) @]"
								(fun ppf unit -> 
									pp ppf "@; @[let ele = sample_%s v in @]" (Path.name y)
									) ()
								(fun ppf unit ->
									pp ppf "@; @[match ele with @. | None -> res @. | Some ele -> (if (!callflag) then fprintf outch (\"Cons#1#%s;%s,\") ele res; ele) @]" "%d" "%d"
									) ()
								xv) ()
								(fun ppf unit -> pp ppf "@[in let _ = fprintf outch (\"\t\")@]") ()
								(fun ppf unit -> 
								pp ppf "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((List.length %s)) @]" (x ^ "_l") "%d" xv
								) ()
						| _ -> 
						(pp formatter "@;@[%a %a %a %a %a %a@]"
							(fun ppf unit -> pp ppf "@[in let _ = fprintf outch (\"%s:\");@]" (x^"_heap")) ()
							(fun ppf unit -> 
								pp ppf "@[in let _ = for ith = 0 to ((List.length %s)-1) do@]" xv
								) ()
							(fun ppf unit -> pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%s@];)@]@]" 
								("(callflag := false)")
								(fun ppf unit -> pp ppf "@[(try fprintf outch (\"Cons#1#%s;%s,\") ((List.nth %s (%s))) ((List.nth %s (%s))) with _->((fprintf outch (\"Cons#1#%s;-1000\") ((List.nth %s (%s))) )))@]"
								"%d" "%d" xv "ith" xv "ith+1" "%d" xv "ith";) ()
								("(callflag := true)")) ()
							(fun ppf unit -> pp ppf "@[%s@]" "done;"
								) ()
							(fun ppf unit -> pp ppf "@[in let _ = fprintf outch (\"\t\")@]") ()
							(fun ppf unit -> 
								pp ppf "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((List.length %s)) @]" (x ^ "_l") "%d" xv
								) ())
					else
						(pp formatter "@;@[%a %a %a %a@]"
							(fun ppf unit -> 
								pp ppf "@[in let _ = for ith = 0 to ((List.length %s)-1) do@]" xv
								) ()
							(fun ppf unit -> pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%a@] @;@[%a@]; @;@[%s@];)@]@]" 
								("(callflag := false)")
								(fun ppf unit -> pp ppf "@[fprintf outch (\"%s:\")@]" x) ()
								(fun ppf unit -> pp ppf
										"@[fprintf outch (\"%s#%s,\") ((%s)); @]"
										(x ^ "_" ^ "0") "%d" "ith") ()
								(fun ppf unit -> pp ppf "@[(try fprintf outch (\"%s#%s\\t\") ((List.nth %s %s)) with _->(fprintf outch (\"\t\")))@]"
								(x ^ "_" ^ "r") "%d" xv "ith";) ()
								("(callflag := true)")) ()
							(fun ppf unit -> pp ppf "@[%s@]" "done"
								) ()
							(** print the length of  *)
							(fun ppf unit -> 
								pp ppf "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((List.length %s)) @]" (x ^ "_l") "%d" xv
								) ())
				| Fconstr (y,fs,_,_,_) when y = Predef.path_list -> (** only print list length *)
					if !(Clflags.reachability) then
						()
					else
						(pp formatter "@;@[%a@]"
						(** print the length of  *)
							(fun ppf unit -> 
								pp ppf "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((List.length %s)) @]" (x ^ "_l") "%d" xv
								) ())		
				| Fconstr (y,_,_,_,_) when y = Predef.path_array -> (* printing array *)
					(* Considering for-all paired array property, array dumping could be complex *)
					(*let arr_pairs = detect_arr_adj_pattern bad_constraint allbindings (*Fixme:sound?*)[] in
					try
						let () = List.find (fun (pe, _) -> pe = pexpr) arr_pair in
						
					with _ ->*)
					let print_arr x y z (* x: the array reference; y: the display name of the array; z: index *) = 
						(pp formatter "@;@[%a %a %a@]"
						(fun ppf unit -> 
							pp ppf "@[in let _ = for ith = 0 to ((Array.length %s)-1) do@]" x
							) ()
						(fun ppf unit -> pp ppf "@;@[if @[(!callflag) then (@;@[%s@]; @;@[%a@]; @;@[%a@] @;@[%a@]; @;@[%a@]; @;@[%s@];)@]@]" 
							("(callflag := false)")
							(fun ppf unit -> pp ppf "@[fprintf outch (\"%s:\")@]" y) ()
							(fun ppf unit -> pp ppf
									"@[fprintf outch (\"%s#%s,\") ((%s)); @]"
									(z ^ "_" ^ "0") "%d" "ith") ()
							(fun ppf unit -> pp ppf "@[(try fprintf outch (\"%s#%s,\") ((%s.(%s))) with _->(fprintf outch (\"\t\")))@]"
							(z ^ "_" ^ "r") "%d" x "ith";) ()
							(fun ppf unit -> pp ppf "@[(try fprintf outch (\"%s#%s\\t\") ((%s.(%s))) with _->(fprintf outch (\"\t\")))@]"
							(z ^ "_" ^ "r'") "%d" x "(ith-1)";) ()
							("(callflag := true)")) ()
						(fun ppf unit -> pp ppf "@[%s@]" "done"
							) ()) in
					(** Print the tempt array and result array respectively *)
					(print_arr (tempt_arr_prefix^x) (tempt_arr_prefix^x) x; print_arr x (tempt_arr_postfix^x)) x
				| Fconstr (y,_,_,_,_) when Hashtbl.mem measures y -> (* User defined data type *)
					let measures = Hashtbl.find measures y in
					(List.iter (fun (m, recflag) -> 
						if recflag then
							let m = Path.name m in
							pp formatter "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((%s)) @]" 
							(x ^ "_" ^ m) "%d" (m ^ " " ^ xv)
					) measures;
					(** Print linkability *)
					if !(Clflags.reachability) then
						pp formatter "@;@[in let _ = if (!callflag) then (fprintf outch (\"%s:\"); ignore (%s); fprintf outch (\"\\t\")) @]" 
							(x ^ "_heap") ("sample_" ^ (Path.name y) ^ " " ^ xv)
					)
				| Fconstr (y,_,_,_,_) when Path.same y Predef.path_bool -> 
					pp formatter "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") (if (%s) then 1 else 0) @]" x "%d" xv
				| Farrow _ -> (** Higher order function return dump *)
					(synthesize formatter x bad_constraint (locals) defs allbindings)
				| Fconstr _ | Fvar _ -> (** First class value dump *) 
					pp formatter "@;@[in let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((%s)) @]" x "%d" xv
				| Frecord _ -> assert false
				| Ftuple (fs, _) -> 
					let pats = Array.to_list (Array.init (List.length fs) (fun i -> 
						Tpat_var (Ident.create_persistent (
							x^"."^(string_of_int i)) ))) in
					List.iter2 (fun p f -> 
						ignore(dump formatter env fenv measures p f bad_constraint locals defs allbindings (List.length fs))
					) pats fs
				| _ -> (** Fixme for more support  *)
				 (pp Format.err_formatter 
					"Value cannot be dumped for %s \n" x; 
					flush stderr; assert false) 
				)
			| None -> (print_string "Value cannot be dumped \n"; assert false) 			
		) patfrs; patfrs)

(** dump the function by making up an instruction *)
let rec dump_fun_Info formatter env fenv measures fr bad_constraint locals defs allbindings n_args = 
	match fr with
		| Farrow (Some pat, f1, f2, _) -> (
			(** dump the parameter *)
			let locals' = dump formatter env fenv measures pat f1 bad_constraint locals defs allbindings n_args in
			(** dump the return *)
			let locals = (*Common.remove_duplicates*) (locals @ locals') in
			dump_fun_Info formatter env fenv measures f2 bad_constraint locals defs allbindings (n_args-1))
		| _ -> (** dump the return only *) 
			(ignore (dump formatter env fenv measures (Tpat_var (Ident.create "r")) fr bad_constraint locals defs allbindings (n_args+1));
			pp formatter "@;@[in let _ = if (!callflag) then fprintf outch (\"\\n\") @]";
			pp formatter "@[%s@]" "in r"; n_args)
			
let myFind tbl compare = 
	match (Hashtbl.fold (fun k v res -> match res with
		| None -> if (compare k = 0) then Some (k, v) else None
		| Some _ -> res) tbl None) with
		Some r -> r
		| None -> assert false
		
(** add instructions so that array can be copied and dumped *)
let dump_side_effects formatter se_env lookup_fr funlocation funname env fenv = 
	let (funpath, fr) = 
		try 
			let (funpath, _) = Env.lookup_value (Lident funname) env in
			let fr = Hashtbl.find se_env.funframebindings funpath in
			(funpath, fr)
		with Not_found ->
			myFind se_env.funframebindings (fun k -> String.compare (Path.name k) funname) in
	let allbindings = Frame.get_fun_bindings env fr in
	(* If an array is encountered copy it into a temp variable and output the temp *)
	let arrs = List.fold_left (fun resarrs (p, f) -> match f with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_array -> resarrs @ [p]
		| _ -> resarrs
	) [] allbindings in
	List.iter (fun arr -> 
		let arrname = Path.name arr in
		pp formatter "@[let %s = Array.copy %s in @]" (tempt_arr_prefix^arrname) arrname
	) arrs	

let complete_funcall_info = Hashtbl.create 3

(** add instructions so that function x can be dumped*)
let dump_fun formatter se_env lookup_fr funlocation funname env fenv exptxt = 
	if (is_measure se_env funname) then pp formatter "@[%s@]" "in r" else 
	(*let _ = Printf.printf "funname=%s\n" funname in*)
	let n_args = count_arguments exptxt in
	let _ = pp formatter "@;@[in let _ = if (!callflag) then fprintf outch (\"name:%s\\t\") %s @]" 
						"%s" ("\"" ^ funname ^ "\"") in
	let measures = se_env.measures in
	try 
		let (funpath, _) = Env.lookup_value (Lident funname) env in
		let fr = (*Lightenv.find funpath fenv in *)
			Hashtbl.find se_env.funframebindings funpath in
		let (bad_constraint, defs) = 
			try (Hashtbl.find se_env.badbindings funpath,
					Hashtbl.find se_env.fundefenvs funpath) 
			with _ -> (
				if !(se_env.dty) then
				({pre = Predicate.Not Predicate.True; post = Predicate.Not Predicate.True}, 
				try Hashtbl.find se_env.fundefenvs funpath with _ -> (
					Format.fprintf Format.std_formatter "%s unfound@." (Path.name funpath); assert false))
				else (Format.fprintf Format.std_formatter "%s unfound@." (Path.name funpath); assert false)) in
		let locals = [] in
		(** locals are function parameters while
			defs are defined in the context of function definition *)
		let allbindings = Frame.get_fun_bindings env fr in
		let n_args' = dump_fun_Info formatter env fenv measures fr bad_constraint locals defs allbindings n_args in
		if (n_args' < 0) then (Hashtbl.replace complete_funcall_info funname n_args)
	with Not_found -> (** Possibly local function not in env or fenv *)
		(*try let fr = lookup_fr funlocation in
		dump_fun_Info formatter env fenv fr with _ -> assert false*)
		let (funpath, fr) = myFind se_env.funframebindings 
							(fun k -> String.compare (Path.name k) funname) in
		let (bad_constraint, defs) = 
			try (Hashtbl.find se_env.badbindings funpath,
					Hashtbl.find se_env.fundefenvs funpath) 
			with _ -> (({pre = Predicate.Not Predicate.True; post = Predicate.Not Predicate.True}, 
				try Hashtbl.find se_env.fundefenvs funpath with _ -> assert false)
			) in
		let locals = [] in
		(** locals are function parameters while
			defs are defined in the context of function definition *)
		let allbindings = Frame.get_fun_bindings env fr in
		let n_args' = dump_fun_Info formatter env fenv measures fr bad_constraint locals defs allbindings n_args in
		if (n_args' < 0) then (Hashtbl.replace complete_funcall_info funname n_args)

(** add instructions so that function application at apploc can be dumped *)				
let dump_app formatter se_env apploc exptxt = 
	let (fname, n_args) = count_app_arguments exptxt in
	if (not (is_measure se_env fname)) (*&& (is_user_function se_env fname)*) then (
	let sys_def_vars = 
		["guard"; "assert_guard"; "pattern"; "hoencoding"; "arraygetencoding"; 
		"fixarrayencoding"; "ho_pattern"; "envrefreshed"; "lstlenencoding"; "dtyencoding"] in
	let measures = se_env.measures in
	if (Hashtbl.mem se_env.funcallenvs apploc) then
		let context = Hashtbl.find se_env.funcallenvs apploc in
		(pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"env:%s\\t\") \"%s\" in @]"
					"%s" (Location.tostring apploc);
		List.iter (fun (path, fr) -> 
		let pathname = Path.name path in 
		match fr with
		| Fconstr (x,_,_,_,_) when x = Predef.path_unit -> ()
		| Fconstr (x,_,_,_,_) when x = Predef.path_array -> ()
		| Fconstr (x,_,_,_,_) when Hashtbl.mem measures x -> ()
		| Fconstr (x,_,_,_,_) when x = Predef.path_list -> 
			if (List.for_all (fun var -> (String.compare var pathname != 0)) sys_def_vars) then
				pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((List.length %s)) in @]" 
				(pathname ^ "_l") "%d" pathname
		| Farrow _ -> ()
		| Fconstr (x,_,_,_,_) when x = Predef.path_bool ->
			if (List.for_all (fun var -> (String.compare var pathname != 0)) sys_def_vars) then
				pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") (if (%s) then 1 else 0) in @]" 
				pathname "%d" pathname
		| Fconstr _ | Fvar _ -> (** First class value dump *) 
			if (List.for_all (fun var -> (String.compare var pathname != 0)) sys_def_vars) then
				pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"%s:%s\\t\") ((%s)) in @]" 
				pathname "%d" pathname
		| Frecord _ | Ftuple _ -> ()
		| _ -> (** Fixme for more support  *)
		 (Format.fprintf Format.std_formatter 
			"Value cannot be dumped for %s \n" pathname; 
			assert false) 
		) context;
		pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"\\n\") in @]")
	else 
		if (Hashtbl.mem complete_funcall_info fname) then
			let n_args' = Hashtbl.find complete_funcall_info fname in
			if (n_args = n_args') then 
				(pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"env:%s\\t\") \"%s\" in @]"
					"%s" (Location.tostring apploc);
				pp formatter "@;@[<hov2>let _ = if (!callflag) then fprintf outch (\"\\n\") in @]")
		else () )
	
		
(** Define a local read function for helper *)		
let read_log () = 
	let lines = ref [] in
	let chan = open_in log_fname in
	try
  	while true; do
    	lines := input_line chan :: !lines
  	done; []
	with End_of_file ->
  	close_in chan;
  List.rev !lines ;;		
		
let read_dumpings pos_samples = 
	let logs = read_log () in
	let env_stack = Stack.create () in
	List.iter (fun log -> 
		(*let _ = pp Format.err_formatter 
			"log =  %s @." log in*)
		let bindings = Str.split (Str.regexp "[ \t]+") log in
		let (namebinding, value_bindings) = 
			(List.hd bindings, List.tl bindings) in
		(*let _ = pp Format.err_formatter 
			"namebinding =  %s @." namebinding in*)
		let name = Str.split (Str.regexp ":") namebinding in
		let (name_name, name_value) = (List.hd name, List.nth name 1) in
		if (String.compare name_name "env" = 0) then
			Stack.push (name_value, value_bindings) env_stack
		else
			let data = (*List.map (Str.split (Str.regexp ":")) para_return_bindings*) 
				value_bindings in
			let (loc, env_value_bindings) = Stack.pop env_stack in
			let loc = loc ^ "_" ^ name_value in
			if (Hashtbl.mem pos_samples loc) then
				let (_, samples) = Hashtbl.find pos_samples loc in
				Hashtbl.replace pos_samples loc (name_value, samples@[(data, env_value_bindings)])
			else
				Hashtbl.replace pos_samples loc (name_value, [(data, env_value_bindings)])
		) logs
	
				

		
		