open Printf

let c = ref 0

let dump_locals outch locals = 
	let locals = List.map (fun (n, v) -> 
		if (String.contains v ',') then
			let sub_locals = Str.split (Str.regexp ",") v in
			List.map (fun sub_local -> 
				let sub_name = Str.split (Str.regexp "#") sub_local in
				(List.hd sub_name, List.nth sub_name 1)
				) sub_locals
		else [(n, v)]
		) locals in
	let locals = List.flatten locals in
	let _ = fprintf outch "1" in
	let names = List.fold_left (fun res (name_name, name_value) -> 
		(** the real dump of values *)
		((try fprintf outch " %d" (int_of_string name_value) 
		with _ -> 
			(Format.fprintf Format.std_formatter "%s:%s \n" name_name name_value;
			assert false));
		res @ [name_name])
		) [] locals in
	(fprintf outch "\n"; names)
	
(** split good samples: just see one line from good and return how to do the partition *)
let split_large_goods goods = 
	if (List.length goods = 0) then assert false
	else
		let (locals, _) = List.hd goods in
		let plains = ref [] in
		let hos = ref [] in
		let _ = List.iter (fun local -> 
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			if (String.contains name_value ',') then
				(hos := (name_name::(!hos)))
			else 
				(plains := (name_name::(!plains)))
			) locals in
		let hos = Common.remove_duplicates (!hos) in
		let plains = Common.remove_duplicates (!plains) in
		if (List.length hos = 0) then [plains]
		else 
			let groups = 
				List.map (fun ho -> 
					ho :: (plains)
					) (hos) in
		 	groups

(** Ugly. This function is just a copy of dump_good but considers
 * only variables mentioned in {names}.
 *)			
let dump_small_goods goods names = 
	let outch = open_out ("./good.mat") in
	match (List.fold_left (fun res (locals, _) -> 
		(let tbl = Hashtbl.create 9 in
		let _ = List.iter (fun local -> 
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			(** This is the only difference to dump_goods! *)
			if (List.exists (fun n -> (String.compare n name_name = 0)) names) then 
				if (Hashtbl.mem tbl name_name) then
					Hashtbl.replace tbl name_name ((Hashtbl.find tbl name_name) @ [name_value])
				else Hashtbl.replace tbl name_name [name_value]
			) locals in
		let locals = Hashtbl.fold (fun k vs res -> 
			res @ [(List.map (fun v -> (k, v)) vs)]) tbl []	in	
		let locals = Misc.lflap locals in
		let names = List.fold_left (fun res local -> 
			let names = (dump_locals outch local) in		
			match res with
				| Some res -> Some res
				| None -> Some names) res locals in
		match res with
			| Some res -> Some res
			| None -> names
		)) None goods) with
		| Some names -> (close_out outch; names)
		| _ -> (close_out outch; assert false)			

(** dump goods into good.mat *)
let dump_goods goods = 
	let outch = open_out ("./good.mat") in
	match (List.fold_left (fun res (locals, _) -> 
		(let tbl = Hashtbl.create 9 in
		let _ = List.iter (fun local -> 
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			if (Hashtbl.mem tbl name_name) then
				Hashtbl.replace tbl name_name ((Hashtbl.find tbl name_name) @ [name_value])
			else Hashtbl.replace tbl name_name [name_value]
			) locals in
		let locals = Hashtbl.fold (fun k vs res -> 
			res @ [(List.map (fun v -> (k, v)) vs)]) tbl []	in	
		let locals = Misc.lflap locals in
		let names = List.fold_left (fun res local -> 
			let names = (dump_locals outch local) in		
			match res with
				| Some res -> Some res
				| None -> Some names) res locals in
		match res with
			| Some res -> Some res
			| None -> names
		)) None goods) with
		| Some names -> (close_out outch; names)
		| _ -> (close_out outch; assert false)

(** dump bads into bad.mat *)
let dump_bads bads names = 
	let onebad = try List.map fst (List.hd bads) with _ -> assert false in
	let _ = List.iter (fun b -> Format.fprintf Format.std_formatter "b=%s@." b) onebad in
	(*let _ = assert (List.length names = List.length onebad) in*)
	let indices = List.map (fun name -> 
		let index = ref (-1) in
		(ignore(List.fold_left (fun i b -> 
			if (String.compare b name = 0) then (index := i);
			(i+1)) 0 onebad);
		if (!index = -1) then 
			(Format.fprintf Format.std_formatter "this %s is unfound@." name;
			assert false)
		else !index
		)
		) names in	
	let outch = open_out ("./bad.mat") in
	let _ = (c := 1 + (!c)) in
	(List.iter (fun bad -> (
		for i = 0 to ((List.length names)-1) do
			(*let _ = (Format.fprintf Format.std_formatter "name %s: \n" (List.nth names i)) in*)
			let index = List.nth indices i in
			(*let _ = (Format.fprintf Format.std_formatter "index %d: \n" index) in*)
			let (_, value) = List.nth bad index in
			(*let _ = (Format.fprintf Format.std_formatter "value %d: \n" value) in *)
			if (i = (List.length names - 1)) then
				fprintf outch ("%d\n") value
			else fprintf outch ("%d ") value
			done
		)) bads;
	close_out outch)

(** dump slopes into w.mat *)
(** Fixme: we firtly use the default slopes *)
let dump_slopes n = ()

(** Define a local read function for learner *)		
let read_learning_result () = 
	let lines = ref [] in
	let chan = open_in "./hyperplanedata.txt" in
	try
  	while true; do
    	lines := input_line chan :: !lines
  	done; []
	with End_of_file ->
  	close_in chan;
  List.rev !lines ;;

(** read learning result from hyperplanedata.txt *)
let read_invariant names =
	let half = ref [] in
	let pos = ref [] in
	let neg = ref [] in
	let lines = read_learning_result () in
	let _ = List.fold_left (fun flag line -> 
		if (String.length line >= 10 && String.compare (Str.string_before line 10) "halfspaces" = 0) then 1
		else if (String.length line >= 6 && String.compare (Str.string_before line 6) "posvec" = 0) then 2
		else if (String.length line >= 6 && String.compare (Str.string_before line 6) "negvec" = 0) then 3
		else if (flag = 1) then (half := ((!half)@[line]); flag)
		else if (flag = 2) then (pos := ((!pos)@[line]); flag)
		else if (flag = 3) then (neg := ((!neg)@[line]); flag)
		else 
			(Format.fprintf Format.std_formatter "reading %s \n" line;
			assert false)
		) 0 lines in
	let pexprs = List.map (fun one -> 
		let paras = Str.split (Str.regexp "[ \t]+") one in
		let paras = List.rev (List.tl (List.rev paras)) in
		let paras = List.map int_of_string paras in
		let (c, paras) = (List.hd paras, List.tl paras) in
		List.fold_left (fun (res,i) para -> 
			if (para = 1) then 
				(Predicate.Binop (res, Predicate.Plus, Predicate.Var (Path.mk_ident (List.nth names i))), i+1)
			else if (para = -1) then
				(Predicate.Binop (res, Predicate.Minus, Predicate.Var (Path.mk_ident (List.nth names i))), i+1)
			else if (para = 0) then
				(res, i+1)
			else
				(Predicate.Binop (res, Predicate.Plus, 
					Predicate.Binop (Predicate.PInt para, Predicate.Times, Predicate.Var (Path.mk_ident (List.nth names i)))
				), i+1)
			) ((Predicate.PInt c), 0) paras
		) (!half) in
	let pexprs = List.map fst pexprs in
	let preds = List.map (fun one -> 
		let conjuncts = Str.split (Str.regexp "[ \t]+") one in
		let conjuncts = List.rev (List.tl (List.rev conjuncts)) in
		let conjuncts = List.map int_of_string conjuncts in
		Predicate.big_and (Misc.mapi (fun conjunct i -> 
			if (conjunct > 0) then Predicate.Atom (List.nth pexprs i, Predicate.Ge, Predicate.PInt 0)
			else Predicate.Atom (List.nth pexprs i, Predicate.Lt, Predicate.PInt 0)
			) conjuncts)
		) (!pos) in
	Predicate.big_or preds

(** learning invariant *)
let learn pos_samples neg_samples invariants = 
	(*let functions = Hashtbl.fold (fun f fr res -> (f, fr)::res) se_env.funbindings [] in*)
	Hashtbl.iter (fun path bads -> 
		if (List.length bads = 0) then
			(Format.fprintf Format.std_formatter "%s is always good! \n" (Path.name path))
		else 
			let fname = Path.name path in
			let _ = (Format.fprintf Format.std_formatter "Generate invariant for %s: \n" fname) in
			(** put goods in good.mat *)
			let goods = Hashtbl.fold (fun _ (name, dumpings) res -> 
				if (String.compare name fname = 0) then
					res @ dumpings
				else res
				) pos_samples [] in
			
			(*let names = dump_goods goods in 	*)	
			(** Split samples into different clusters depending upon the number of higher order funtions  *)	
			let namess = split_large_goods goods in
			List.iter (fun names -> 
				(** put goods in good.mat *)
				let names = dump_small_goods goods names in	
				let _ = List.iter (fun name -> 
					(Format.fprintf Format.std_formatter "ordering name: %s\n" name)
					) names in
				(** put bads in bad.mat *)
				let _ = dump_bads bads names in
				let _ = (Format.fprintf Format.std_formatter "good/bad prepared \n") in
				(** put the corresponding slopes *)	
				let _ = dump_slopes (List.length names) in 
				let _ = Run.learn () in
				let	invariant = read_invariant names in
				if (Hashtbl.mem invariants path) then
					(Hashtbl.replace invariants path (invariant::(Hashtbl.find invariants path)))
				else Hashtbl.replace invariants path [invariant]
				) namess	
		) neg_samples