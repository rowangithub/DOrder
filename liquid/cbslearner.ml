(** This is the proposed constraint-based learner.
 * It takes postive/negative samples as input and returns candidate invariant
 *)
open Types
open Predicate
open Backwalker
open Modelsolver

type learning_result =
{ ipre : (bool * (string, Path.t) Hashtbl.t * (Predicate.t list * Predicate.t)) list;
ipost : (bool * (string, Path.t) Hashtbl.t * (Predicate.t list * Predicate.t)) list}

(** Stats *)
let nb_hypo = ref 0

let return_repr = "r"

(* counte the iteration of learning *)
let counter = ref 0

exception NoSample

let triple_snd (a, b, c) = b 

(**************************
 *** Learning Algorithm ***
 **************************)

let bind_name_to_path names tbl = 
	List.iter (fun name -> 
		if (Hashtbl.mem tbl name) then ()
		else Hashtbl.replace tbl name (Path.mk_ident name)) names 

let transl_samples samples tbl = 
	try List.map (fun sample ->
		List.map (fun (name, value) -> 
			(Hashtbl.find tbl name, Predicate.PInt value)) sample
		) samples with _ -> assert false

(** Fixme: templates by input? *)	
let genPredicates fpath tbl templates =
	let paths = Hashtbl.fold (fun _ path res -> (res@[path])) tbl [] in
	let templates = (PAtom (PVar paths, [Gt;], PVar paths)) :: (
		List.fold_left (fun res (op, constant) -> 
			res @ [(PAtom (PVar paths, [op;], PPInt [constant]))]) [] templates
		) in
	(* Something special --> Need more tests to confirm the usefulness *)
	(** Heuristic: Find the constants of this procedure and build relations *)	
	let constants = Backwalker.get_constants fpath in	
	let constants = List.filter (fun c -> c >= 4) constants in
	let templates = 
		if (constants = []) then templates 
		else 
			let (min, max, constanst) = (List.hd constants, List.hd constants, List.tl constants) in	
			let (min, max) = List.fold_left (fun (min, max) c -> 
				if (c < min) then (c, max) 
				else if (c > max) then (min, c)
				else (min, max)
				) (min, max) constants in
			if (max > min) then 
				let d = max - min in
				[PAtom (PVar paths, [Le;], PBinop (PVar paths, [Plus], PPInt [d]))] @ templates 
			else templates in
	(*let templates = [
		PAtom (PVar paths, [Gt;], PVar paths); 
		PAtom (PVar paths, [Ge;], PPInt [0]);
		PAtom (PVar paths, [Le;], PPInt [0])] in*)
	let predicates = List.fold_left (fun res template -> 
		res @ (Qualdecl.gen_preds template)) [] templates in    
	let predicates = List.filter (fun predicate -> match predicate with
		| Atom (e1, _ ,e2) -> 
			let vars1 = exp_vars e1 in
			let vars2 = exp_vars e2 in
			(List.for_all (fun var1 -> (
				List.for_all (fun var2 -> not (Path.same var1 var2)) vars2
				)) vars1)
		| _ -> assert false
		) predicates in
	Common.remove_customized_duplicates (fun p1 p2 -> match (p1, p2) with
		| (Atom (e1, Eq, e2), Atom (e1', Eq, e2')) -> 
			(((e1 = e1') && (e2 = e2')) || ((e1 = e2') && (e2 = e1')))
		| _ -> p1 = p2
		) predicates

(* Synthesize a set of atomic predicates plus with those given by the users *)	
(** Compare whether 2 predicates are equivalent *)
let comparePred p1 p2 = match (p1, p2) with
	| (Predicate.Atom (_,Predicate.Eq,_), Predicate.Atom (_,c,_))
	| (Predicate.Atom (_,c,_), Predicate.Atom (_,Predicate.Eq,_)) when c <> Predicate.Eq -> false
	| (p1, p2) ->
		let p1vars = Predicate.vars p1 in
		let p2vars = Predicate.vars p2 in
		if (List.length p1vars <> List.length p2vars) then false
		else if (List.exists (fun p1var -> 
			List.for_all (fun p2var -> not (Path.same p1var p2var)) p2vars) p1vars) then false
		else if (List.exists (fun p2var -> 
			List.for_all (fun p1var -> not (Path.same p2var p1var)) p1vars) p2vars) then false
		else (* use a theorem prover call to decide whether p1 and p2 are equivalent *)
			TheoremProver.unsat (Predicate.Not (
				Predicate.And (Predicate.implies (p1, p2), Predicate.implies (p2, p1))
				(*Predicate.Or (
					Predicate.And (Predicate.implies (p1, p2), Predicate.implies (p2, p1)),
					Predicate.And (Predicate.implies (Predicate.Not p1, p2), Predicate.implies (p2, Predicate.Not p1))) *)
				)) 

let trans_preds tbl predicates = 
	(*let _ = Format.fprintf Format.std_formatter "atomic len = %d@." (List.length predicates) in
	let _ = Hashtbl.iter (fun str _ -> Format.fprintf Format.std_formatter "str = %s@." str) tbl in*)
	Common.map_partial (fun pred -> 
		let pred = Predicate.map_expr (fun expr -> match expr with
			| Predicate.FunApp (fn, args) -> (
				assert (List.length args = 1);
				let arg = Predicate.exp_var (List.hd args) in
				Predicate.Var (Path.mk_ident ((Path.name arg) ^ "_" ^ fn)))
			| expr -> expr) pred in
		try let vars = Predicate.vars pred in
		let substs = List.map (fun var -> 
			(var, Predicate.Var (Hashtbl.find tbl (Path.name var)))) vars in
		Some (Predicate.apply_substs substs pred)
		with Not_found -> ((*Format.fprintf Format.std_formatter "pred %a failed atomic gen@." Predicate.pprint pred;*) None)
	) predicates
	
let genAtomicPredicates flag fpath tbl enforcements predicates = 
	let res = trans_preds tbl predicates in
	(** Generate relations among return values and inputs *)
	(*let outputrelations = 
		if flag then []
		else List.fold_left (fun res force -> 
		if (Hashtbl.mem tbl force) then
			let force = Hashtbl.find tbl force in
			Hashtbl.fold (fun key path res -> 
				if (List.for_all (fun force' -> String.compare key force' <> 0) enforcements)	then
					res @ [Predicate.Atom (Predicate.Var force, Predicate.Ge, Predicate.Var path); 
					Predicate.Atom (Predicate.Var force, Predicate.Le, Predicate.Var path); 
					Predicate.Atom (Predicate.Var force, Predicate.Ge, Predicate.Binop (Predicate.Var path, Predicate.Plus, Predicate.PInt 1)); 
					Predicate.Atom (Predicate.Var force, Predicate.Le, Predicate.Binop (Predicate.Var path, Predicate.Plus, Predicate.PInt 1))]
				else res
			) tbl res
		else res	
	) [] enforcements in*)
	let outputrelations = [] in
	(** Generate relations among the inputs *)
	(*let constants = Backwalker.get_constants fpath in
	let (_, inputrelations) = Hashtbl.fold (fun key path (prevars, res) -> 
		if (List.for_all (fun force' -> String.compare key force' <> 0) enforcements) then
			let res' = List.fold_left (fun res prevar -> 
				List.fold_left (fun res c -> 
					if (c = 0) then res @ [Predicate.Atom (Predicate.Var prevar, Predicate.Ge, Predicate.Var path)]
					else res @ [Predicate.Atom (Predicate.Binop (Predicate.Var prevar, Predicate.Minus, Predicate.Var path), Predicate.Le, Predicate.PInt c)] 
					@ [Predicate.Atom (Predicate.Binop (Predicate.Var prevar, Predicate.Minus, Predicate.Var path), Predicate.Le, Predicate.PInt c)]) res constants	
			) [] prevars in
			(prevars@[path], res@res')
		else (prevars, res)
	) tbl ([], []) in*)
	let inputrelations = [] in
	let preds = Common.remove_duplicates (res@outputrelations@inputrelations) in
	(*let preds = List.sort (fun p1 p2 -> 
		let s1 = Common.remove_duplicates (List.map Pervasives.abs (Predicate.ints p1 @ Predicate.coeffs p1)) in
		let s2 = Common.remove_duplicates (List.map Pervasives.abs (Predicate.ints p2 @ Predicate.coeffs p2)) in
		let s1 = List.fold_left (fun res s -> res+s) 0 s1 in
		let s2 = List.fold_left (fun res s -> res+s) 0 s2 in
		(s1 - s2)
		) preds in*)
	let preds = Common.remove_customized_duplicates (fun pred1 pred2 -> comparePred pred1 pred2) preds in	
	(*let _ = List.iter (fun pred -> Format.fprintf Format.std_formatter "Guessed atomics = %a@." Predicate.pprint pred) preds in*)
	preds
 
	

let eval_sample predicates sample =
	List.map (fun predicate -> 
		let predicate = Predicate.apply_substs sample predicate in
		let result = TheoremProver.implies (Predicate.True) predicate in
		(TheoremProver.finish (); if result then 1 else 0)
		) predicates
	
let build_constraint pos_sample neg_sample cvars = 
	Predicate.big_or (Common.map3 (fun pos neg cvar -> 
		(*Atom (Binop (PInt pos, Times, Var cvar), Ne, Binop (PInt neg, Times, Var cvar))*)
		And (Atom (PInt pos, Ne, PInt neg), Atom (Var cvar, Eq, PInt 1))
		) pos_sample neg_sample cvars) 		
		
(** Try to return all solutions *)
let rec allsolution solutions constraint_f cvars = 
	(*let constraint_f = Predicate.And (constraint_f, Atom (sumpexpr, Eq, PInt k)) in*)
	let restriction = List.fold_left (fun res solution -> 
		let l = List.fold_left (fun res cvar -> 
			let value = 
				try Hashtbl.find solution cvar with _ -> assert false in
			if (value = 1) then res@[Predicate.Atom (Predicate.Var cvar, Predicate.Ne, Predicate.PInt 1)]
			else res
		) [] cvars in
		(assert (List.length l > 0); res @ [Predicate.big_or l])
	) [] solutions in
	let _ = assert (List.length restriction > 0) in
	let constraint_f = Predicate.And (constraint_f, Predicate.big_and restriction) in
	let newsolution = TheoremProver.model constraint_f 1 in
	if (List.length newsolution = 0) then solutions
	else 
		let new_solution = List.hd newsolution in
		allsolution (solutions@[new_solution]) constraint_f cvars

let rec_solve constraint_f cvars = 
	(*let sum solution = 
		List.fold_left (fun sum cvar -> 
			let xi = try Hashtbl.find solution cvar with _ -> assert false in
			sum + xi
			) 0 cvars in*)
	let sumpexpr = List.fold_left (fun res cvar -> 
		Binop (res, Plus, Var cvar)
		) (PInt 0) cvars in
	let rec loop (*old_solution*) k =
		let cf = Predicate.And (constraint_f, Atom (sumpexpr, Eq, PInt k)) in
		let solution = TheoremProver.model cf 1 in
		if (List.length solution = 0) then loop (k+1)
		else (List.hd solution, k) in
		(*let cf = Predicate.And (constraint_f, Atom (sumpexpr, Lt, PInt k)) in
		let solution = TheoremProver.model cf 1 in
		if (List.length solution = 0) then (old_solution, k)
		else 
			let solution = List.hd solution in 
			let k = sum solution in
			loop solution k in*)
	let init_solution = TheoremProver.model constraint_f 1 in
	let (solution, k) = 
		if (List.length init_solution = 0) then (Hashtbl.create 0, 0) 
		else 
			(*let init_solution = List.hd init_solution in
			let k = sum init_solution in
			let _ = Format.fprintf Format.std_formatter "init_k = %d\n" k in*)
			Bstats.time "minimize_k" (loop (*init_solution*)) 1 in
	let solutions = 
		if (k > 0) then (*[solution]*)
			Bstats.time "allsolution" (allsolution [solution] 
				(Predicate.And (constraint_f, Atom (sumpexpr, Eq, PInt k))) ) cvars 
		else [] in
	(*let _ = Format.fprintf Format.std_formatter "Solution size = %d@." (List.length solutions) in*)
	List.map (fun solution ->
		fst (List.fold_left (fun (res, i) cvar -> 
		let value = 
			try Hashtbl.find solution cvar with _ -> assert false in
		if (value = 1) then (res@[i], i+1)
		else (res, i+1)
		) ([], 0) cvars)
	) solutions
	
let to_dnf_expression pos_samples neg_samples = 
	let rec loop pos_sample neg_samples res = 
		if (List.length neg_samples = 0) then res
		else 
			(* find a column that distinguish the most number of neg_samples *)
			let (i, negs) = List.fold_left (fun (resi, resnegs) i -> 
				let pvalue = List.nth pos_sample i in
				let negs = List.filter (fun neg_sample -> 
					let nvalue = List.nth neg_sample i in
					nvalue <> pvalue
					) neg_samples in
				if (List.length negs > List.length resnegs) then (i, negs)
				else (resi, resnegs)
			) (-1, []) (Array.to_list (Array.init (List.length pos_sample) (fun i -> i))) in	
			let _ = assert (0 <= i && i < (List.length pos_sample)) in
			(* add the column to res *)
			let res = res @ [i] in
			(* Minus the covered neg_samples and continue this loop *)
			let neg_samples = List.filter (fun neg_sample -> 
				List.for_all (fun neg -> neg <> neg_sample) negs	
			) neg_samples in
			loop pos_sample neg_samples res in
	let pos_samples = List.map (fun pos_sample -> 
		let res = loop pos_sample neg_samples [] in
		let _ = assert (List.length res > 0) in
		Misc.mapi (fun pv i -> 
			let u = List.exists (fun resi -> resi = i) res in
			if u then 
				if pv = 1 then `True else `False
			else `Dontcare	
		) pos_sample
	) pos_samples in
	Common.remove_duplicates pos_samples
	(*List.map (fun pos_sample -> 
		Misc.mapi (fun pv i -> 
			let u = List.for_all (fun neg_sample -> 
				pv = (List.nth neg_sample i)
				) neg_samples in
			if u then `Dontcare
			else
			if pv = 1 then `True else `False) pos_sample	
	) pos_samples	*)
	
(* We invoke Quine-McCluskey algorithm to interprete the separator *)			
let interprete solution predicates pos_samples neg_samples =
	let pos_samples = List.map (fun pos_sample -> 
		List.map (fun i -> List.nth pos_sample i) solution
		(*List.fold_left (fun (res,i) pos ->
			if (List.exists (fun i' -> i = i') solution) then (res @ [pos], i+1)
			else (res, i+1)
			) ([], 0) pos_samples*)
		) pos_samples in
	let pos_samples = Common.remove_duplicates pos_samples in
	let neg_samples = List.map (fun neg_sample -> 
		List.map (fun i -> List.nth neg_sample i) solution
		) neg_samples in
	let neg_samples = Common.remove_duplicates neg_samples in
	let dnfexp = to_dnf_expression pos_samples neg_samples in
	let (dnfexps, res) = Bes.auto_optimize dnfexp in	
	if (res) then
		Predicate.big_or (List.map (fun dnfexp -> 
			Predicate.big_and (Common.map_partial (fun x->x) (Misc.mapi (fun pv i ->
				let predicate = List.nth predicates (List.nth solution i) in
				if (pv = `True) then Some predicate 
				else if (pv = `False) then Some (Not predicate)
				else None
				) dnfexp))
			) dnfexps)
	else assert false
	(*Predicate.big_or (List.map (fun pos_sample -> 
		Predicate.big_and (Misc.mapi (fun pv i ->
			let predicate = List.nth predicates (List.nth solution i) in
			if (pv = 1) then predicate else (Not predicate)
			) pos_sample)
		) pos_samples) *)
		
(***********************************************)
(*************** Util Funcitons ****************)
(***********************************************)
let remove_nth l indices = 
	fst (List.fold_left (fun (l',index) li -> 
		if (List.exists (fun i -> i = index) indices) 
		then (l', index+1)
		else (l'@[li], index+1)	
	) ([],0) l)
	
(********************************************************************* 
    Invariant Mining Engine:
 		-- An (arbitrary) invariant is inferred from very general template 
		-- we use n to indicate the number of disjunctions 
**********************************************************************)
let invmine_learn n pos_samples neg_samples tbl enforcements env ffr =
	let predicates = Invmine.learn n pos_samples neg_samples tbl enforcements env ffr in
	let _ = (nb_hypo := !nb_hypo + List.length predicates) in
	let _ = Format.fprintf Format.std_formatter 
		"Refining the generated invariants from template because they need to separate bad samples@." in
	(* Pick a subset of res that can separate the negative samples *)			
	(** names are better with paths in the favor of the implementation *)
	if (List.length neg_samples > 0 && n <> 0) then
		let pos_samples = transl_samples pos_samples tbl in
		let neg_samples = transl_samples neg_samples tbl in	
		let pos_samples = List.map (eval_sample predicates) pos_samples in
		let neg_samples = List.map (eval_sample predicates) neg_samples in
		
		(** Before preceding to learning we infer the common predicates *)
		let commons = ref [] in
		let _ = 
			for iter = 0 to ((List.length predicates)-1) do
				match (List.fold_left (fun res sample -> match res with
					| Some v ->
						let v' = List.nth sample iter in
						if (v = v') then Some v else None
					| None -> None 	
				) (Some (List.nth (List.hd pos_samples) iter)) (pos_samples@neg_samples)) with
					| Some v -> 
						if (v = 1) then (commons:=(iter,(List.nth predicates iter))::(!commons)) 
						else (commons:=(iter,(Predicate.Not (List.nth predicates iter)))::(!commons))
					| None -> ()
				done;
		in
		let (common_indices, commons) = List.split (!commons) in
		(** Before go to the constraint solving system, remove commons from both predicates, pos and neg_samples *)
		let predicates = remove_nth predicates common_indices in
		let pos_samples = List.map (fun pos_sample -> remove_nth pos_sample common_indices) pos_samples in
		let neg_samples = List.map (fun neg_sample -> remove_nth neg_sample common_indices) neg_samples in
		(** Go to the constraint solving system *)
		let cvars = Misc.mapi (fun _ i -> Path.mk_ident ("x_" ^ (string_of_int i))) predicates in
		let cvar_bounds = List.map (fun cvar -> 
			Predicate.Or
				(Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 1),
				Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 0))
			) cvars in
		let constraints = 
			List.flatten (List.map (fun pos_sample -> 
				List.map (fun neg_sample -> build_constraint pos_sample neg_sample cvars) neg_samples
				) pos_samples) in
		let constraint_f = Predicate.big_and (cvar_bounds @ constraints) in
		let solutions = Bstats.time "rec_solve" (rec_solve constraint_f) cvars in
		(*let _ = List.iter (fun i -> Format.fprintf Format.std_formatter "solution i = %d\n" i) solution in*)
		let separators = List.map (fun solution ->
			interprete solution predicates pos_samples neg_samples
		) solutions in
		let _ = List.iter (fun separator -> 
			Format.fprintf Format.std_formatter "Separator := %a@." (Predicate.pprint') separator	
		) separators in
		List.map (fun sep -> ([], sep)) separators
	else List.map (fun pred -> ([], pred)) predicates
	(* Learing completed in two phases: invemine + minimize *)	

(*******************************
******CDNF learning engine******
********************************)

(** 0, 0, ... should be put into every partition *)
let post_process_partitions flag partitions =	
	if (flag || Hashtbl.length partitions <> 2) then ()
	else 
		let parts = Hashtbl.fold (fun k v res -> res @ [(k, v)]) partitions [] in
		let (part0, partn0) = List.partition (fun (_, vs) -> 
			List.exists (fun v -> List.for_all (fun (_, v') -> v' = 0) v) vs) parts in 
		let partn0 = List.map (fun (k, v) -> 
			let s = List.hd v in
			let s = List.map (fun (n, _) -> (n, 0)) s in
			(k, v@[s])) partn0 in
		List.iter (fun (k, v) -> Hashtbl.replace partitions k v) (part0 @ partn0)	
		
			
let is_measure_variable e measure = 
	let measure = Path.name measure in
	String.length e > String.length measure && 
		String.compare (Str.last_chars e ((String.length measure)+1)) ("_"^measure) = 0			
		
let isjunk predicates = 
	predicates = [] ||
	List.for_all (fun pred -> match pred with
		| Predicate.Atom (_,Predicate.Eq,_) when (List.length (Predicate.vars pred) <= 1) -> true
		| _ -> false
		) predicates		

module Int : Graph.Sig.COMPARABLE with type t = int =
struct
   type t = int 
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end
module G = Graph.Imperative.Digraph.Concrete(Int)
module PC = Graph.Gpath.Check(G)
module DotGraph =
struct
   type t = G.t
   module V = G.V
   module E = G.E
   let iter_vertex = G.iter_vertex
   let iter_edges_e = G.iter_edges_e
   let graph_attributes g = []
   let default_vertex_attributes g = [`Shape `Box]
   let vertex_name i = Printf.sprintf "V_%d" i 
   let vertex_attributes v = [`Label (vertex_name v)]
   let default_edge_attributes g = []
   let edge_attributes e = []
   let get_subgraph v = None
end
module Dot = Graph.Graphviz.Dot(DotGraph) 
let dump_graph g = 
  (Dot.fprint_graph Format.std_formatter g;
	Format.fprintf Format.std_formatter "@.")

let makegraph d links = 	
	let g = G.create () in
	let nodes1, nodes2 = Hashtbl.fold (fun (cons, index) nodes (res1, res2) -> 
		List.fold_left (fun (res1, res2) (u, v) -> 
			((*Format.fprintf Format.std_formatter "add an edge from %d to %d@." u v;*)
			(* Dont save (-1000) nodes *)
			if (u = -1000) then assert false
			else if (v = -1000) then 
				if (u >= 1000) then (res1, u::res2)
				else (u::res1, res2)
			else (G.add_edge g u v; 
				if (u >= 1000 && v >= 1000) then (res1, u::v::res2)
				else if (u >= 1000) then (v::res1, u::res2)
				else if (v >= 1000) then assert false
				else u::v::res1, res2))
		) (res1, res2) nodes
	) links ([], []) in
	(nodes1, nodes2, g)
	
module SampleSet = Set.Make (
	struct
		type t = int list
		let compare = compare
	end);;	
	
let of_list l = 
	List.fold_left (fun res e -> SampleSet.add e res) SampleSet.empty l	
	
(** Solve the shape constriants *)
let mini_invariants predicates pos_samples neg_samples = 
	(*let _ = Format.fprintf Format.std_formatter "minimizing ...@." in
	let _ = List.iter (fun predicate ->
		Predicate.pprint Format.std_formatter predicate
		) predicates in
	let _ = Format.fprintf Format.std_formatter "@.===============@." in
	let _ = List.iter (fun p ->
		List.iter (fun v ->
		Format.fprintf Format.std_formatter "%d  " v
		) p; Format.fprintf Format.std_formatter "@.") pos_samples in
	let _ = Format.fprintf Format.std_formatter "@.=============@." in
	let _ = List.iter (fun n ->
		List.iter (fun v ->
		Format.fprintf Format.std_formatter "%d  " v
		) n; Format.fprintf Format.std_formatter "@.") neg_samples in
	let _ = Format.fprintf Format.std_formatter "@." in*) 
	if (pos_samples = [] || neg_samples = []) then []
	else
	(** link (t, u, v), link (t, v, u) and reach (t, u) and reanch (t, v) have better not come
			simultaneously *)
	let (pairs, _, _) = List.fold_left (fun (res, pre, ind) predicate ->
		match (predicate, pre) with
			| (Predicate.Link (t, c, i, u, v), Predicate.Link (t', c', i', u', v')) when
				(t = t' && c = c' && i = i' && u = v' && v = u') -> 
				(res @ [(ind-1,ind)], predicate, ind+1)
			(*| (Predicate.Reach (t, u), Predicate.Reach (t', v)) when 
				(t = t' && u = Datatype.forall_vexpr && v = Datatype.forall_uexpr) -> 
				(res @ [(ind-1,ind)], predicate, ind+1) *)
			| _ -> (res, predicate, ind+1)
		) ([], Predicate.True, 0) predicates in	
	(** Go to the constraint solving system *)
	let cvars = Misc.mapi (fun _ i -> Path.mk_ident ("x_" ^ (string_of_int i))) predicates in
	let cvar_bounds = List.map (fun cvar -> 
		Predicate.Or
			(Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 1),
			Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 0))
		) cvars in
	let cvar_pairs = (List.map (fun (i, j) -> 
		let ci = List.nth cvars i in
		let cj = List.nth cvars j in
		Predicate.Atom (Predicate.Binop (Predicate.Var ci,Predicate.Plus,Predicate.Var cj), 
											Predicate.Le, Predicate.PInt 1)
		) (pairs)) in
	let constraints = 
		List.flatten (List.map (fun pos_sample -> 
			List.map (fun neg_sample -> build_constraint pos_sample neg_sample cvars) neg_samples
			) pos_samples) in
	let constraint_f = Predicate.big_and (cvar_bounds @ cvar_pairs @ constraints) in
	let solutions = Bstats.time "rec_solve" (rec_solve constraint_f) cvars in
	(*let _ = List.iter (fun solution -> List.iter (
		fun i -> Format.fprintf Format.std_formatter "%d " i) solution;
		Format.fprintf Format.std_formatter "@.") solutions in*)
	solutions

(** typing atomic predicates *)
let kind_of pred = 
	(* Link _ -> 2; reach (_, u) -> 0; reach (_, v) -> 1; u = *; v = * -> 0; otherwise -> (-1) *)
	match pred with
		| Predicate.Link _ -> 2
		| Predicate.Reach (_, u) 
			when (u = Datatype.forall_uexpr || u = Datatype.forall_vexpr)	-> 1
		| pred when (List.exists (fun var -> 
				var = Datatype.forall_uvar || var = Datatype.forall_vvar) (Predicate.vars pred)) -> 0
		| pred -> (-1)	

(** Learning through CDNF for heap domain *)
let isreturn p = (*Path.same p Frame.returnpath*)
	List.mem Frame.returnpath (Predicate.exp_vars p)
let rec exp_var p = match p with
	| Predicate.Var p -> Path.name (p)
	| Predicate.Proj (i, p) -> (exp_var p)^"."^(string_of_int i)
	| Predicate.Field (f, p) -> (exp_var p)^"."^f
	| _ -> assert false
			
let find_similarity pure return' returns =
	fst (List.fold_left (fun (res, ind) return -> 
		if return = return' then res@[ind], ind+1
		else
		match return, return' with
			(*| (Predicate.Link (a,cons,index,_,_), Predicate.Link (b,cons1,index1,_,_)) 
				when String.compare cons cons1 = 0 && index = index1 -> 
					(match (a, b) with
						| Predicate.Proj (_, pexpr), Predicate.Proj (_, pexpr') 
						| Predicate.Field (_, pexpr), Predicate.Field (_, pexpr') ->
							if (pexpr = pexpr') then (res@[ind], ind+1)
							else (res, ind+1)
						| _ -> res, ind+1) *)
			| (Predicate.Reach (a,u), Predicate.Reach (b,u1))
				when (u = Datatype.forall_uexpr) && (u = u1) && pure -> 
					(match (a, b) with
						| Predicate.Proj (_, pexpr), Predicate.Proj (_, pexpr') 
						| Predicate.Field (_, pexpr), Predicate.Field (_, pexpr') ->
							if (pexpr = pexpr') then (res@[ind], ind+1)		
							else (res, ind+1)
						| _ -> res, ind+1)
			(*| (Predicate.Atom (u1,Predicate.Eq,b), Predicate.Link (a,_,_,u,_))
			| (Predicate.Link (a,_,_,u,_), Predicate.Atom (u1,Predicate.Eq,b))		*)	
			| (Predicate.Atom (u1,Predicate.Eq,b), Predicate.Reach (a,u))
			| (Predicate.Reach (a,u), Predicate.Atom (u1,Predicate.Eq,b)) ->
				if (u = Datatype.forall_uexpr) && (u = u1) then
					match (a, b) with
						| Predicate.Proj (_, pexpr), Predicate.Proj (_, pexpr') 
						| Predicate.Field (_, pexpr), Predicate.Field (_, pexpr') ->
							if (pexpr = pexpr') then (res@[ind], ind+1)
							else res, ind+1
						| _ -> res, ind+1
				else res, ind+1
			| (return, return') -> res, ind+1
		) ([], 0) returns	)
	
let symmetric_version pred = 
	match pred with
		| Predicate.Atom (v,Predicate.Eq,_) 
		| Predicate.Link (_,_,_,v,_) 
		| Predicate.Reach (_, v) when v = Datatype.forall_vexpr -> true
		| _ -> false

(* Return the index/field of a return *)
let suffixf return =
	match return with
		| Predicate.Reach (Predicate.Proj (i, e), u)
		| Predicate.Atom (Predicate.Proj (i, e), Predicate.Eq, u) 
		| Predicate.Atom (u, Predicate.Eq, Predicate.Proj (i, e)) 
			when (u = Datatype.forall_uexpr || u = Datatype.forall_vexpr) -> i
		| _ -> (-1)

(** The other predicates in Pi_O other than Pi shall be added back to Pi_I *)
let otherreturns pure allreturns currreturns focusreturn = 	
	if (pure) then 
		(* ONLY support record types: reach (r.2, v) should consider reach (r.1, u) and u = r.0 *)
		let suffix = suffixf focusreturn in
		if List.length currreturns > 1 || suffix < 0 then []
		else
			let _ = assert (List.mem Datatype.forall_vvar (Predicate.vars focusreturn)) in
			let focusreturn = Predicate.subst Datatype.forall_uexpr Datatype.forall_vvar focusreturn in
			let others = find_similarity pure focusreturn allreturns in
			List.filter (fun i -> 
				let other = List.nth allreturns i in
				let suffix' = suffixf other in
				(assert (suffix' >= 0); suffix' <> suffix)) others
	else 
		match focusreturn with
			| Predicate.Reach (x, u) -> 
				Common.map_partial (fun x->x) 
					(Misc.mapi (fun return i -> 
						match return with
							| Predicate.Reach (y, v) when u <> v && x <> y ->
								if (List.exists (fun i' -> i = i') currreturns) then None else Some i
							| _ -> None		
					) allreturns) 
			| _ -> []

(** Bound the free varialbes in a predicate with quantifers *)
let find_quantifiers pred = 
	let vars = Predicate.vars pred in
	let u = List.mem (Datatype.forall_uvar) vars in
	let v = List.mem (Datatype.forall_vvar) vars in
	if (u && v) then [Datatype.forall_uvar; Datatype.forall_vvar]
	else if u then [Datatype.forall_uvar]
	else if v then [Datatype.forall_vvar]
	else []
	
(* pure = true learning shape or data invariant *)
(* pure = false learning mixed shape and data invariant *)	
let solve_shape_constraints pure table params returns = 
	let cache = Hashtbl.create (List.length returns) in
	let param_len = List.length params in
	(* 1. work on each returns *)
	fst (List.fold_left (fun (res, index) return -> 	
		let _ = Format.fprintf Format.std_formatter "Focusing on %a@." Predicate.pprint return in
		(* Hack: if return = (u = r) then it should be sent to learning !!*)
		if (Hashtbl.mem cache index || (symmetric_version return && not pure)) then res, index+1
		else
		(* 1.5 find the other returns that are similiar (~record and tuple~) *)
		let similarities = find_similarity pure return returns in
		let returnpred = Predicate.big_or (List.map (fun i -> List.nth returns i) similarities) in
		let _ = List.iter (fun i -> Hashtbl.add cache i ()) similarities in
		(* 2. partition the table by the value of return *)
		let (pos_samples, neg_samples) = 
			(*if (kind_of return > 0) then*)
				(* Study shape properties *)	
				List.partition (fun row ->
					let values = List.map (fun i -> List.nth row (param_len + i)) similarities in
					List.exists (fun value -> value > 0) values
					(*let value = List.nth row (param_len + index) in
					(value > 0) *)
				) table 
			(*else 
				(* Study arithmetic properties *) 
				(table, [])*) in
				
		(** Need to add the rest of returns to the params and hence pos_samples and neg_samples *)		
		let otherreturns = otherreturns pure returns similarities return in
		if (otherreturns = [] && symmetric_version return) then res, index + 1
		else
		let _ = List.iter (fun i -> Hashtbl.add cache i ()) otherreturns in
		(* 3. only reserve params in the table *)
		(*let params = if (kind_of return > 0) then params else params @ returns in*)
		let pos_samples = 
			(*if (kind_of return > 0) then*)
				List.map (fun sample -> Common.sublist 0 (List.length params - 1) sample @
						List.map (fun i -> List.nth sample (param_len+i)) otherreturns ) pos_samples 
			(*else pos_samples*) in
		let neg_samples = 
			(*if (kind_of return > 0) then*)
				List.map (fun sample -> Common.sublist 0 (List.length params - 1) sample @
						List.map (fun i -> List.nth sample (param_len+i)) otherreturns ) neg_samples
			(*else neg_samples*) in 	
		let params = params @ (List.map (fun i -> List.nth returns i) otherreturns) in	
		
		let rminds = match return with
			| Predicate.Link _ -> 
				if (pure) then
					(* the params with kind < 0 don't need consideration *) 
					fst (List.fold_left (fun (res, ind) param ->
						if (kind_of param < 0) then res @ [ind], ind+1 else res, ind+1
					) ([], 0) params)
				else (* drop all predicate that is not u * v *)
					fst (List.fold_left (fun (res, ind) param ->
						if (kind_of param = 0) && (List.exists (fun var ->
							 not (Path.same var Datatype.forall_uvar || Path.same var Datatype.forall_vvar)) 
							(Predicate.vars param))
						then res @ [ind], ind+1 else res, ind+1
					) ([], 0) params)
			| Predicate.Reach _ when (kind_of return = 1) ->
				if (pure) then
					(* the params with kind < 0 don't need consideration *) 
					(* the params with kind = 2 don't need consideration *)
					if (symmetric_version return) then
						(* the params with kind < 0 don't need consideration *) 
						fst (List.fold_left (fun (res, ind) param -> match param with
							| Predicate.Reach (t, _) when not (isreturn t) -> res @ [ind], ind+1
							| otherwise ->
								if (kind_of param < 0) then res @ [ind], ind+1 else res, ind+1
						) ([], 0) params)
					else
						fst (List.fold_left (fun (res, ind) param -> match param with
							| Predicate.Link _ -> res @ [ind], ind+1
							| param when (List.mem (Datatype.forall_vvar) (Predicate.vars param)) -> res @ [ind], ind+1
							| _ -> if (kind_of param < 0) then res @ [ind], ind+1 else res, ind+1
						) ([], 0) params)
				else (* drop all predicate that is u * v *) 
					(*fst (List.fold_left (fun (res, ind) param ->
						if (kind_of param = 0) && (List.for_all (fun var ->
							 (Path.same var Datatype.forall_uvar || Path.same var Datatype.forall_vvar)) 
							(Predicate.vars param))
						then res @ [ind], ind+1 else res, ind+1
					) ([], 0) params) *) []
			| return when (kind_of return = 0) -> (* This is useful to learn treemax t = r *)
				(match return with
					| Predicate.Atom (_,Predicate.Eq,_) -> 
						(assert pure;
						(* the params with kind < 0 don't need consideration *) 
						fst (List.fold_left (fun (res, ind) param -> match param with
							| Predicate.Reach (t, _) when symmetric_version return && not (isreturn t) -> res @ [ind], ind+1
							| otherwise ->
								if (kind_of param < 0) then res @ [ind], ind+1 else res, ind+1
						) ([], 0) params))
					| return -> assert false)
			| return -> (* kind_of return = -1 *)
				(* the params with kind >= 0 don't need consideration *)
				fst (List.fold_left (fun (res, ind) param -> 
					if (kind_of param >= 0) then res @ [ind], ind+1
					else res, ind+1
				) ([], 0) params)  in
		let params = remove_nth params rminds in
		let pos_samples = Common.remove_duplicates (
			List.map (fun pos_sample -> remove_nth pos_sample rminds) pos_samples) in
		let neg_samples = Common.remove_duplicates (
			List.map (fun neg_sample -> remove_nth neg_sample rminds) neg_samples) in 
		(**  Important --> Complete the unreachable samples for shape properties only *)	
		let additionals = 
			if (kind_of return >= 0 && pure) then 
				Misc.lflap (List.map (fun param -> 
				match param with
					| Predicate.Link (_,_,_,u,v) when 
						(u = Datatype.forall_uexpr || u = Datatype.forall_vexpr ||
						v = Datatype.forall_uexpr || v = Datatype.forall_vexpr) -> [0]
					| Predicate.Reach (_,u) when 
						(u = Datatype.forall_uexpr || u = Datatype.forall_vexpr) -> [0]
					| _ -> [(*1;*) 0]
				) params)
			else [] in		
		let additionals = match return with
			| Predicate.Link _ when pure -> 
				let reachinds = fst (List.fold_left (fun (res, ind) param -> match param with
					| Predicate.Reach (_, u) when (u = Datatype.forall_uexpr || u = Datatype.forall_vexpr) -> 
						res @ [ind], ind+1
					| Predicate.Atom _ when kind_of param = 0 -> res @ [ind], ind+1
					| _ -> res, ind+1
					) ([], 0) params) in
				additionals @ (List.flatten (List.map (fun reachind -> 
					List.map (fun additional -> (* the element in reachind is set to 1 *)
						Misc.mapi (fun a i -> if i = reachind then 1 else a) additional
						) additionals 
					) reachinds))
			| Predicate.Reach _ when pure && symmetric_version return ->
				let reachinds = fst (List.fold_left (fun (res, ind) param -> match param with
					| Predicate.Reach (_, u) when (u = Datatype.forall_uexpr || u = Datatype.forall_vexpr) -> 
						res @ [ind], ind+1
					| Predicate.Atom _ when kind_of param = 0 -> res @ [ind], ind+1
					| _ -> res, ind+1
					) ([], 0) params) in
				(List.flatten (List.map (fun reachind -> 
					List.map (fun additional -> (* the element in reachind is set to 1 *)
						Misc.mapi (fun a i -> if i = reachind then 1 else a) additional
						) additionals 
					) reachinds)) 
			| _ -> additionals in
		let neg_samples = neg_samples @ additionals in	
		(**  Negative samples are completed *)					
		let pos_set = of_list pos_samples in
		let neg_set = of_list neg_samples in
		let inter_set = SampleSet.inter pos_set neg_set in
		
		let separators =
			(* 4. check if there exsits duplicates in the poss and negs *)	
			if (SampleSet.cardinal neg_set = 0) then
				(* This is tailored for reach (t, u) or reach (t, x) *)
				match return with
					| Predicate.Reach (_, u) when kind_of return = 1 ->
						let _ = assert (not pure) in
						let (preds, _) = List.fold_left (fun (res, i) param -> 
							if (List.mem (Predicate.exp_var u) (Predicate.vars param) &&
								List.for_all (fun sample -> List.nth sample i > 0) pos_samples) 
							then res @ [param], i + 1
							else res, i + 1
						) ([], 0) params in
						if (preds = []) then []
						else 
							let pre = Predicate.big_and preds in
							let foralls = find_quantifiers pre in
							[Predicate.Forall (foralls, Predicate.implies (returnpred, pre))]
					| Predicate.Reach (_, r) ->  
						let _ = assert pure in if (isreturn r) then [return] else []
					| _ -> assert false
			else if (SampleSet.cardinal inter_set = 0 && pure) then
				let solutions = mini_invariants params pos_samples neg_samples in
				List.map (fun solution ->
					let pre = interprete solution params pos_samples neg_samples in
					let foralls = find_quantifiers pre in
					Predicate.Forall (foralls, 
					Predicate.And 
						(Predicate.implies (pre, returnpred), Predicate.implies (returnpred, pre)))
				) solutions
			else
				let pos_set = SampleSet.diff pos_set inter_set in
				let neg_set = SampleSet.diff neg_set inter_set in
				let pos_samples1 = SampleSet.elements pos_set in
				let neg_samples1 = SampleSet.elements neg_set in
				let solutions1 = 
					if pure && not (symmetric_version return) then
						mini_invariants params pos_samples1 neg_samples 
					else [] in
				let solutions2 = mini_invariants params pos_samples neg_samples1 in
				let separators1 = 
					List.map (fun solution ->
						let pre = interprete solution params pos_samples1 neg_samples in
						let foralls = find_quantifiers pre in
						Predicate.Forall (foralls, 
						Predicate.implies (pre, returnpred))
						) solutions1 in
				let separators2 = 
					List.map (fun solution ->
						let pre = interprete solution params pos_samples neg_samples1 in
						let pre = match return with
							| Predicate.Atom (u, Eq, _) when u = Datatype.forall_uexpr ->
								let boundv = Predicate.Not (List.find (fun param -> match param with
									| Predicate.Reach (_, v) when v = Datatype.forall_vexpr -> true
									| _ -> false) params) in
								Predicate.Or (boundv,
									Predicate.Or (Predicate.Atom (Datatype.forall_uexpr, 
																Predicate.Eq, Datatype.forall_vexpr), pre))
							| Predicate.Atom (v, Eq, _) when v = Datatype.forall_vexpr ->
								let boundu = Predicate.Not (List.find (fun param -> match param with
									| Predicate.Reach (_, u) when u = Datatype.forall_uexpr -> true
									| _ -> false) params) in
								Predicate.Or (boundu, pre)
							| return -> pre in
						let foralls = find_quantifiers pre in
						Predicate.Forall (foralls, 
						Predicate.implies (returnpred, pre))
						) solutions2 in
				separators1 @ separators2 in
		let _ = List.iter (fun separator -> 
			Format.fprintf Format.std_formatter "Solve separator = %a@." (Predicate.pprint') separator	
		) separators in	
		res @ separators, index + 1		
	) ([], 0) returns)

let path_check graph links cons index u v = 
	if (Hashtbl.mem links (cons, index)) then
		let pairs = Hashtbl.find links (cons, index) in
		try 
			let neibor_of_u = List.assoc u pairs in
			(*let _ = Format.fprintf Format.std_formatter "neibor of u is %d@." neibor_of_u in*)
			PC.check_path graph neibor_of_u v 
		with _ -> false
	else false 
	
let heap_cdnf_learn path heap_samples tbl enforces env fr udt_table = 
	if not !(Clflags.reachability) || enforces = [] then [] 
	else
	(*1. Construct a set of atomic predicates*)
	let _ = Format.fprintf Format.std_formatter "the size of heap_samples = %d@." (List.length heap_samples) in
	let oallbindings = Frame.get_fun_bindings env fr in
	let allbindings = Common.expand (fun (p, f) -> match f with
		| Frame.Frecord (_,ts,_) -> 
			((List.map (fun (t, field, _) -> (Predicate.Field (field, p), t)) ts, []))
		| Frame.Ftuple (ts, _) -> 
			((Misc.mapi (fun t i -> (Predicate.Proj (i, p), t)) ts, []))
		| Frame.Fconstr _ 
		| Frame.Fvar _ -> ([], [(p, f)])
		| _ -> ([], [])
		) (List.map (fun (p, f) -> (Predicate.Var p, f)) oallbindings) [] in
	let allbindings = List.rev allbindings in
	let is_heap_prog = List.exists (fun (_, f) -> match f with
		| Frame.Fconstr (ty, _, _, _, _) -> 
				(Hashtbl.mem udt_table ty || Path.same ty Predef.path_list)
		| f -> false) allbindings in
	if not is_heap_prog then []
	else
	let (params, returns, _, plains) = 
		List.fold_left (fun (resparams, resreturns, resobjs, resscalars) (p, f) -> 
		match f with
			| Frame.Fconstr (ty, _, _, _, _) when 
				(Hashtbl.mem udt_table ty || Path.same ty Predef.path_list)-> 
				(*let _ = Format.fprintf Format.std_formatter "ty = %s@." (Path.name ty) in*)
				let (_, ty) = Datatype.extract_value_type_from_container_fr f in 
				(*let _ = Format.fprintf Format.std_formatter "ty' = %s@." (Path.name ty) in*)
				if (isreturn p && enforces = []) then (resparams, resreturns, resobjs, resscalars)
				else
					let alllinks = 
						if (Path.same ty Predef.path_list) then [("cons", 1)]
						else
							let declaration = Hashtbl.find udt_table ty in
							let allparams = match declaration.type_kind with 
								| Type_variant decs -> (decs)
								| kind -> assert false in	
							(* Type of allparams : (string * type_expr list) list *)
							(* establish a link from l to r or r to l ... *)
							List.fold_left (fun res (cstrname, params) -> 
								(fun (x,_,_) -> x) (List.fold_left (fun (res, reslinks, index) param -> 
									match param.desc with
									| Tconstr (p, _, _) -> 
										let (b, p') = Datatype.extract_value_type_from_container_ty (param.desc) in
										if (b || Path.same ty p') then (
											res @ [(cstrname, index)] @ (List.map (fun reslink -> 
												(cstrname, int_of_string ((string_of_int index) ^ (string_of_int reslink)))
												) reslinks), 
											reslinks@[index], index+1
										)
										else (res, reslinks, index+1)
									| _ -> (res, reslinks, index+1)) (res, [], 0) params)
								) [] allparams in
					if (isreturn p) then
						(resparams, (List.map (fun (cstrname, index) -> 
						Predicate.Link ((*Predicate.Var*) p, String.lowercase cstrname, index, 
							Datatype.forall_uexpr, Datatype.forall_vexpr)
						) alllinks) @ (
							(** if p is of record type, then we generate the symmtric version *)
							(match p with
								| Predicate.Field _
								| Predicate.Proj _ -> [Predicate.Reach ((*Predicate.Var*) p, Datatype.forall_uexpr);
																			Predicate.Reach ((*Predicate.Var*) p, Datatype.forall_vexpr)]
								| _ -> [Predicate.Reach ((*Predicate.Var*) p, Datatype.forall_uexpr)]
							) @ 
							((List.map (fun s -> (Predicate.Reach (p, s))) resscalars) @ resreturns)), resobjs @ [p], resscalars)
					else
					(((List.map (fun (cstrname, index) -> 
						Predicate.Link ((*Predicate.Var*) p, String.lowercase cstrname, index, 
							Datatype.forall_uexpr, Datatype.forall_vexpr)
						) alllinks) @
					(List.map (fun (cstrname, index) -> 
						Predicate.Link ((*Predicate.Var*) p, String.lowercase cstrname, index, 
							Datatype.forall_vexpr, Datatype.forall_uexpr)
						) alllinks) @
					([Predicate.Reach ((*Predicate.Var*) p, Datatype.forall_uexpr);
						Predicate.Reach ((*Predicate.Var*) p, Datatype.forall_vexpr)]
						@ (List.map (fun s -> (Predicate.Reach ((*Predicate.Var*) p, (*Predicate.Var*) s))) resscalars) 
						@ resparams)), resreturns, resobjs @ [p], resscalars)
					(*(Predicate.Link (Predicate.Var p, "node", 0, Datatype.forall_uexpr, Datatype.forall_vexpr))::
					(Predicate.Link (Predicate.Var p, "node", 2, Datatype.forall_uexpr, Datatype.forall_vexpr))::res*)
			| Frame.Fconstr (ty, _, _, _, _) when Path.same ty Predef.path_bool -> 
				if (isreturn p && enforces = []) then (resparams, resreturns, resobjs, resscalars)
				else 
					if isreturn p then (resparams, resreturns @ [Predicate.Bool p], resobjs, resscalars)
					else (resparams @ [Predicate.Bool p], resreturns, resobjs, resscalars) 
			| f -> 
				if (isreturn p && enforces = []) then (resparams, resreturns, resobjs, resscalars)
				else 
					if isreturn p then
						(resparams, resreturns @ (
							(** if p is of record type, then we generate the symmtric version *)
							match p with
								| Predicate.Field _
								| Predicate.Proj _ -> [Predicate.Atom (Datatype.forall_uexpr,Predicate.Eq,(*Predicate.Var*) p);
																				Predicate.Atom (Datatype.forall_vexpr,Predicate.Eq,(*Predicate.Var*) p)]
								| _ -> [Predicate.Atom (Datatype.forall_uexpr,Predicate.Eq,(*Predicate.Var*) p);]
							) @ (List.map (fun obj -> Predicate.Reach (obj, p)) resobjs), resobjs, resscalars @ [p])
					else
						(resparams @ [
							Predicate.Atom (Datatype.forall_uexpr,Predicate.Eq,(*Predicate.Var*) p);
							Predicate.Atom (Datatype.forall_vexpr,Predicate.Eq,(*Predicate.Var*) p)] @
							(List.map (fun obj -> Predicate.Reach (obj, p)) resobjs), resreturns, resobjs, resscalars @ [p])
		) 
		([], [], [], []) allbindings in	
	let atomics = params @ returns in
	(* we also use the scalar variables to construct a set of data predicates*)
	let datapreds = 
		Predicate.Atom (Datatype.forall_uexpr, Predicate.Le, Datatype.forall_vexpr) ::
		Predicate.Atom (Datatype.forall_vexpr, Predicate.Le, Datatype.forall_uexpr) ::
		List.flatten (List.map (fun plain -> [Predicate.Atom (Datatype.forall_uexpr, Predicate.Le, plain);
			Predicate.Atom (plain, Predicate.Le, Datatype.forall_uexpr)]) plains) in
	let _ = (nb_hypo := !nb_hypo + List.length atomics + List.length datapreds) in
	(*2. Construct the samples and evalute them using the atomic predicates*)
	let samples = List.map (fun (scalars, structures) -> 
		(*let _ = Format.fprintf Format.std_formatter "===== a new heap sample =====@." in 
		let _ = Hashtbl.iter (fun n p -> Format.fprintf Format.std_formatter "tbl name = %s " n) tbl in
		let _ = Format.fprintf Format.std_formatter "@." in
		let _ = List.iter (fun (n, v) -> Format.fprintf Format.std_formatter "%s:%d@." n v) scalars in
		let _ = Hashtbl.iter (fun d links -> 
			Hashtbl.iter (fun (cons, index) nodes -> 
				List.iter (fun (u, v) -> 
					Format.fprintf Format.std_formatter "%s has %s.%d link from %d to %d@."
					d cons index u v	
				) nodes
			) links
		) structures in	*)
		(* 2.1 Collect all the nodes in the heap and heap structure for each datastructure *)
		let graphs = Hashtbl.create (Hashtbl.length structures) in
		let nodes = Hashtbl.create (Hashtbl.length structures) in
		let virtualnodes = Hashtbl.create (Hashtbl.length structures) in
		let allnodes = Hashtbl.fold (fun d links res -> 
			let (nodes', virtualnodes', graph) = makegraph d links in
			let nodes' = Common.remove_duplicates nodes' in
			let vritualnodes' = Common.remove_duplicates virtualnodes' in
			(*let _ = Format.fprintf Format.std_formatter "Graph built with %d nodes@." (List.length nodes') in
			let _ = dump_graph graph in*)
			let checker = PC.create graph in
			let _ = Hashtbl.replace graphs d checker in
			let _ = Hashtbl.replace nodes d nodes' in
			let _ = Hashtbl.replace virtualnodes d vritualnodes' in
			res @ nodes'
			) structures [] in
		let allnodes = Common.remove_duplicates allnodes in 
		(* 2.2 Generate all pairs of nodes *)	
		let pairs = Common.extract 2 allnodes in
		List.fold_left (fun res pair ->
			let u = List.nth pair 0 in
			let v = List.nth pair 1 in 
			(graphs, nodes, virtualnodes, structures, ("u",u)::("v",v)::scalars) ::
			(graphs, nodes, virtualnodes, structures, ("u",v)::("v",u)::scalars) :: res) [] pairs
	) heap_samples in
	let samples = List.flatten samples in
	(*3. Evaluate the samples (obtain Boolean samples) *)
	let datasamples = List.map (fun (graphs, nodes, virtualnodes, structures, sample) -> 
		(*let _ = Format.fprintf Format.std_formatter "work on this sample: @." in
		let _ = List.iter (fun (n, v) -> Format.fprintf Format.std_formatter "%s:%d " n v) sample in 
		let _ = Format.fprintf Format.std_formatter "@." in *)
		let s = List.map (fun atomic -> 
			(*let _ = Format.fprintf Format.std_formatter "check pred %a@." Predicate.pprint atomic in*)
			match atomic with
			| Predicate.Link (t, cons, index, u, v) ->
				let cons = String.capitalize cons in
				let t = (exp_var t) ^ "_heap" in
				let u = List.assoc (Path.name (Predicate.exp_var u)) sample in
				let v = List.assoc (Path.name (Predicate.exp_var v)) sample in
				(*let _ = Format.fprintf Format.std_formatter "check (u, v) as (%d, %d)@." u v in*)
				let result =
				if (Hashtbl.mem graphs t && Hashtbl.mem nodes t) then
					let nodes = Hashtbl.find nodes t in
					let virtualnodes = Hashtbl.find virtualnodes t in
					if (List.mem u nodes && List.mem v nodes) then
						let graph = Hashtbl.find graphs t in
						let links = Hashtbl.find structures t in
						(** case analysis on the value of index *)
						let digits = Common.digits index in
						if (List.length digits = 1) then
							path_check graph links cons index u v
						else
							let (l, r) = List.nth digits 0, List.nth digits 1 in
							let result = List.exists (fun node ->
								(path_check graph links cons l node u) && (path_check graph links cons r node v)
								) (nodes @ virtualnodes) in
							result
						(*if (Hashtbl.mem links (cons, index)) then
							let pairs = Hashtbl.find links (cons, index) in
							try 
								let _ = Format.fprintf Format.std_formatter "neibor found?" in
								let neibor_of_u = List.assoc u pairs in
								let _ = Format.fprintf Format.std_formatter "neibor of u is %d@." neibor_of_u in
								PC.check_path graph neibor_of_u v 
							with _ -> false
						else false *)
					else false
				else false in
				(*let _ = Format.fprintf Format.std_formatter "Predicate %a linkability is %b@." Predicate.pprint atomic result in*)
				result
			| Predicate.Reach (t, u) -> 
				let t = (exp_var t) ^ "_heap" in 
				let u = List.assoc (exp_var u) sample in
				let result = 
				if (Hashtbl.mem nodes t) then
					let nodes = Hashtbl.find nodes t in
					(*let _ = List.iter (fun v -> Format.fprintf Format.std_formatter "node %d " v) nodes in
					let _ = Format.fprintf Format.std_formatter "come here'''@. " in *)
					List.mem u nodes
				else 
					(*let _ = Format.fprintf Format.std_formatter "come here@. " in*)
					false in
				(*let _ = Format.fprintf Format.std_formatter "Predicate %a reachability is %b@." Predicate.pprint atomic result in*)
				result
			| Predicate.Bool bexpr ->
				let exprstr = exp_var bexpr in
				let (_, value) = List.find (fun (n, _) -> String.compare n exprstr = 0) sample in
				(value = 1)
			| _ -> (* evaluated using SMT solver *)
				let predicate = Predicate.map_expr (fun expr -> 
					try 
						let exprstr = exp_var expr in
						let (_, value) = List.find (fun (n, v) -> String.compare n exprstr = 0) sample in
						Predicate.PInt value
					with _ -> expr
					) atomic in
				let result = TheoremProver.implies (Predicate.True) predicate in
				(*let _ = Format.fprintf Format.std_formatter "result = %b@." result in*)
				(TheoremProver.finish (); result)
			) atomics in
		let t = List.map (fun atomic -> 
				let predicate = Predicate.map_expr (fun expr -> 
					try 
						let exprstr = exp_var expr in
						let (_, value) = List.find (fun (n, v) -> String.compare n exprstr = 0) sample in
						Predicate.PInt value
					with _ -> expr
					) atomic in
				let result = TheoremProver.implies (Predicate.True) predicate in
				(TheoremProver.finish (); result)
			) datapreds in
		(s, t)
		) samples in
	(*4. CDNF learning on Boolean samples *)
	(*let additionals = Misc.lflap (List.map (fun atomic -> 
		match atomic with
			| Predicate.Link (_,_,_,u,v) when 
				(u = Datatype.forall_uexpr || u = Datatype.forall_vexpr ||
				v = Datatype.forall_uexpr || v = Datatype.forall_vexpr) -> [false]
			| Predicate.Reach (_,u) when 
				(u = Datatype.forall_uexpr || u = Datatype.forall_vexpr) -> [false]
			| _ -> [true; false]
		) atomics) in
	let samples = samples @ additionals in*)
	
	(************************ Organize samplpes ***************************)
	(*let _ = assert (datasamples <> []) in*)
	let datasamples = Common.remove_duplicates datasamples in
	let datasamples = List.map (fun (sample1, sample2) -> 
		List.map (fun b -> if b then 1 else 0) sample1,
		List.map (fun b -> if b then 1 else 0) sample2) datasamples in
	let samples = List.map fst datasamples in
	let datasamples = List.map (fun (s1, s2) -> s2 @ s1) datasamples in
	
	(*********************** For Learning Shaple Property **************************)
	let samples = Common.remove_duplicates samples in
	(*let _ = List.iter (fun atomic -> 
			Format.fprintf Format.std_formatter "atomic = %a@." Predicate.pprint atomic) (datapreds@atomics) in
	let _ = List.iter (fun sample -> 
		(List.iter (fun b -> Format.fprintf Format.std_formatter "%d " b) sample;
		Format.fprintf Format.std_formatter "@.")
		) datasamples in *)
	let separators1 = solve_shape_constraints true samples params returns in
	let separators1 = List.map (fun sep -> Predicate.approximate sep) separators1 in
	
	(******************* For Learning Shaple and Data Property *********************)
	let separators2 = 
		if (enforces = []) then []
		else 
			(* filter atomics ... *)
			let rminds = fst (List.fold_left (fun (res, ind) atomic -> match atomic with
				| Predicate.Link (_,_,i,v,_) 
				(*| Predicate.Reach (_, v)*) when v = Datatype.forall_vexpr || i > 9 -> res @ [ind], ind+1
				| Predicate.Reach (r, v) when isreturn r && v = Datatype.forall_vexpr -> res @ [ind], ind+1
				| _ -> if (kind_of atomic < 1) then res @ [ind], ind+1 else res, ind+1
				) ([], 0) atomics) in
			let atomics = remove_nth atomics rminds in
			let datasamples = List.map (fun sample ->
					remove_nth sample (List.map (fun rmind -> List.length datapreds + rmind) rminds)
				) datasamples in				
			solve_shape_constraints false datasamples datapreds atomics in 
	
	List.map (fun sep -> ([], sep)
	) (separators1 @ separators2)	
	
	(*let samples = List.map (fun sample -> 
		Array.of_list (false :: (sample))
		) samples in*)	
	(* membership query and equivalence query are answered by tests -- pos_samples *)
	(*let separator = Cdnflearn.learn 
		(List.length atomics)
		(is_member samples) 
		(is_equivalent atomics samples) in
	let separator = BoolFormula.from_boolformula_t separator in
	let separator = interpret_cdnf atomics separator in
	let _ = Format.fprintf Format.std_formatter "separator = %a@." Predicate.pprint separator in 
	(*5. Return the found invariant *)
	if (separator = Predicate.True || separator = Predicate.Not Predicate.True) then []
	else 
		(List.map (fun sep -> ([], Predicate.Forall ([Datatype.forall_uvar; Datatype.forall_vvar], sep))) 
		[ separator])   *)
			
(** Learning through CDNF for integer domain *)	
(* Heuristic: flag is set ---> enforce the cdnf learning *)
let fast_cdnf_learn ni flag path atomics assertions terminations pos_samples neg_samples tbl enforces env fr measures = 
	(*let _ = Format.fprintf Format.std_formatter "atomics size = %d@." (List.length atomics) in
	let _ = List.iter (fun atomic -> Format.fprintf Format.std_formatter "atomic %a@." Predicate.pprint atomic) atomics in*)
	(*let positives = pos_samples in*)
	let simpleinv = [] in				
	(** A CDNF learning for good program invariant *)
	if (Hashtbl.length tbl <= 2 && not flag) then
		( (*invmine_learn 1 pos_samples neg_samples tbl enforces env fr*) simpleinv) 
	else
		(*let _ = List.iter (fun p ->
		let _ = Format.fprintf Format.std_formatter "pos" in
		let _ = List.iter (fun (n, v) -> 
		Format.fprintf Format.std_formatter " %s:%d " n v) p in
		Format.fprintf Format.std_formatter "\n"
		) pos_samples in*)
		(* This algorithm does not need to see any bad samples *)
		(*let neg_samples = transl_samples neg_samples tbl in*)
		(**//////////// Let us find some additional atomics from tests ///////////*)
		(** Not useful: ni = 1 => Find for all invariants; 
				ni = 2 => Find existential eq invariants; 
				ni = 3 => Find existential ineq invariants *)
		(** Useful: partition the loops and obtain =, >=, > invariants from each loop partitions *)		
		(*1. get all conditionals from program source. Fixme. sourceatomics vs. atomics *)
		let sourceatomics = (List.filter (fun atomic -> match atomic with 
					| Predicate.Atom (e, _, f) when e = f -> false
					| _ -> true) atomics) in
		let sourceatomics = trans_preds tbl sourceatomics in 
		let atomics = (Backwalker.find_atomics path flag) in
		let atomics = trans_preds tbl atomics in
		let datatype_args = Hashtbl.fold (fun n p res -> 
			if List.exists (fun measure -> is_measure_variable n measure
				(*Common.str_contains n ("_" ^ (Path.name measure))*)) measures 
					&& List.for_all (fun m -> String.compare n (Frame.returnrepr^ "_"^(Path.name m)) <> 0) measures
			then res @ [p] else res
			) tbl [] in 
		let defaults = 
			if (List.length datatype_args = 2) then	
				[Predicate.Atom (Predicate.Var (List.nth datatype_args 0), Predicate.Ge, 
					Predicate.Var (List.nth datatype_args 1))]
			else [] in
		let atomics = defaults @ (List.filter (fun pred -> 
			match pred with  
				| Predicate.Atom (Predicate.Var _,_,Predicate.Var _) -> 
					let vars = Predicate.vars pred in
					not (List.for_all (fun var -> List.exists (fun var' ->
						let var, var' = Path.name var, Path.name var' in
						String.compare var var' = 0) datatype_args) vars)
				| _ -> true) atomics) in	
		(*2. Mine some other predicates from program source *)			
		let assertions = trans_preds tbl assertions in
		let pos_samples = transl_samples pos_samples tbl in
		let atomics_inf = if (ni = 0 && flag) then [] else 
			let partition_preds = atomics in	
			let partition_preds = 
				if (flag) then
					let terminations = trans_preds tbl terminations in
					(*3. partition predicate does include assertions and loop terminations *)
					if (ni = 2 && List.length partition_preds = 
												List.length terminations + List.length assertions) then
						List.filter (fun atomic -> 
						List.for_all (fun assertion -> (atomic <> assertion)) assertions
					) partition_preds 
					else List.filter (fun atomic -> 
						List.for_all (fun assertion -> (atomic <> assertion)) assertions &&
						List.for_all (fun termination -> (atomic <> termination)) terminations
					) partition_preds 
				else partition_preds in
			let _ = List.iter (fun pred -> 
				Format.fprintf Format.std_formatter "function %s has partition pred: %a@." 
				(Path.name path) Predicate.pprint pred
				) partition_preds in
			let _ = List.iter (fun assertion -> 
				Format.fprintf Format.std_formatter "function %s has assertion pred: %a@." 
				(Path.name path) Predicate.pprint assertion
				) assertions in	
			(*3. partition the samples *)
			let samples = List.map (fun pos_sample -> 
				(eval_sample partition_preds pos_sample, 
				List.map (fun (k,v) -> match v with
					| Predicate.PInt v -> (Path.name k, v)
					| _ -> assert false) pos_sample)) pos_samples in
			let partitions = Hashtbl.create 4 in
			let _ = List.iter (fun (key, sample) -> 
				if (Hashtbl.mem partitions key) then 
					Hashtbl.replace partitions key ((Hashtbl.find partitions key) @ [sample])
				else Hashtbl.replace partitions key [sample]
			) samples in	
			let _ = post_process_partitions flag partitions in
			let _ = Format.fprintf Format.std_formatter "I have %d partitions@." (Hashtbl.length partitions) in
			(*4. find all possible =, >, >= invariants *)
			let dranges = Backwalker.get_constants path in
			let cranges = Backwalker.get_coeffs path in
			let _ = Format.fprintf Format.std_formatter "ni = %d@." ni in
			(if (Hashtbl.length partitions > 1 && ni = 2) then (
				let samples = Hashtbl.fold (fun _ samples res -> res @ samples) partitions [] in
				let samples = Common.remove_duplicates samples in
				let _ = Format.fprintf Format.std_formatter "octagon ... @." in
				let res = Cpredmine.synthesize_octagon path samples cranges dranges tbl enforces in
				let _ = Format.fprintf Format.std_formatter "octagon ed ...@." in res
				) else [])
			@ defaults @ 
			(let res = 
				Hashtbl.fold (fun key pos_samples res -> 
					let _ = Format.fprintf Format.std_formatter "partition sample size = %d@." (List.length pos_samples) in
					res @ (
						if flag then 
								Cpredmine.synthesize (Hashtbl.length partitions) path pos_samples cranges dranges tbl enforces
						else Cpredmine.synthesize_datatype_constraints path pos_samples cranges dranges tbl enforces)
				) partitions [] in 
			if (isjunk res) then
				let _ = Format.fprintf Format.std_formatter "junk invaraints from partitions@." in
				let samples = Hashtbl.fold (fun _ samples res -> res @ samples) partitions [] in
				let samples = Common.remove_duplicates samples in
				(Cpredmine.synthesize 1 path samples cranges dranges tbl enforces) @ res
			else res) in
   (*let atomics = 
			if (ni = 1 || ni = 2 || ni = 3) then
				let dranges = Backwalker.get_constants path in
				(List.filter (fun atomic -> match atomic with 
					| Predicate.Atom (e, _, f) when e = f -> false
					| _ -> true) atomics) @ 
				(Cpredmine.synthesize ni path pos_samples dranges tbl enforces)
			else (List.filter (fun atomic -> match atomic with 
					| Predicate.Atom (e, _, f) when e = f -> false
					| _ -> true) atomics) in*)
		
		(* New code: *)
		let _ = List.iter (fun pred -> 
				Format.fprintf Format.std_formatter "Has atomicinf pred: %a@." 
				Predicate.pprint pred
				) atomics_inf in
		let source_r, source_p = List.partition (fun atomic -> 
				List.exists (fun var -> 
					String.compare (Path.name var) (Path.name (Frame.returnpath)) = 0 ||
					Common.str_contains (Path.name var) ((Path.name (Frame.returnpath)) ^ ".")
					) (Predicate.vars atomic)
			) sourceatomics in			
			
		let source_p = trans_preds tbl source_p in
		let atomics = trans_preds tbl atomics in
		let source_r = trans_preds tbl source_r in
		let atomics_inf = trans_preds tbl atomics_inf in
		let assertions = trans_preds tbl assertions in
		
		(*-- remove the params that are equal to assertions and inferred predicates  --*)
		let source_p = List.filter (fun source -> 
			List.for_all (fun assertion -> (source <> assertion)) assertions &&
			List.for_all (fun atomic -> comparePred source atomic = false) atomics_inf &&
			List.for_all (fun atomic -> comparePred source (Not atomic) = false) atomics_inf
			) source_p in
		let atomics = List.filter (fun param -> 
			List.for_all (fun assertion -> (param <> assertion)) assertions 
		) atomics in 
		
		let params = genAtomicPredicates flag path tbl enforces (source_p @ atomics) in
		let returns = genAtomicPredicates flag path tbl enforces (source_r @ atomics_inf @ assertions) in	
		(*-- remove the returns that are equal to param --*)
		let returns = List.filter (fun return -> 
			List.for_all (fun param -> comparePred return param = false) params	
		) returns in 
		
		let params, returns = 
			if returns = [] && List.length params > 1 then 
				Common.sublist 0 (List.length params - 2) params, 
				[List.nth params (List.length params - 1)]
			else if params = [] && List.length returns > 1 then
				[List.hd returns], List.tl returns 
			else params, returns in 
			
		let predicates = params @ returns in
		let _ = (nb_hypo := !nb_hypo + List.length predicates) in
		let pos_samples = List.map (eval_sample predicates) pos_samples in
		(* Begin comment old CDNF stuff  *)
		(*let allatomics = sourceatomics @ atomics @ atomics_inf in 
		let predicates = genAtomicPredicates flag path tbl enforces allatomics in
		let _ = Format.fprintf Format.std_formatter "number of inferred predciates = %d@." (List.length predicates) in 
		(*let assertions = trans_preds tbl assertions in
		let pos_samples = transl_samples pos_samples tbl in*)
		(*5. Translate samples to boolean range *)
		let pos_samples = List.map (eval_sample predicates) pos_samples in
		(*******Make statistic summarization of the predicates***********)
		let _ = Format.fprintf Format.std_formatter "####The total number of samples = %d@." (List.length pos_samples) in*)
		(* End commment old CDNF stuff *)
		
		let predstatistics = Misc.mapi (fun pred i -> 
			let pos = List.filter (fun sample -> List.nth sample i = 1) pos_samples in
			let poslen = List.length pos in
			let _ = Format.fprintf Format.std_formatter "####pred = %a with a count as %d@." 
				Predicate.pprint pred poslen in
			(i, poslen)	
			) predicates in
		let (fulls, emptys) = List.fold_left (fun (fulls, emptys) (i, len) -> 
			if (len = 0 || 
				((List.length pos_samples / len > 10 || len = 1)
				&& (List.for_all (fun assertion -> not (assertion = List.nth predicates i)) assertions)
				&& (List.for_all (fun atomic -> match atomic with
					| Predicate.Atom (_,Predicate.Eq,_) -> true
					| atomic -> not (atomic = List.nth predicates i)) atomics))) 
			then (fulls, emptys@[i]) 
			else if (len = List.length pos_samples) then (fulls@[i], emptys)
			else (fulls, emptys)	
		) ([], []) predstatistics in
		let fullseps = List.map (fun i -> List.nth predicates i) fulls in
		(* New code: *)
		let emptys = if flag then emptys else [] in
		let pos_samples = List.map (fun pos_sample -> 
			remove_nth pos_sample (fulls@emptys)) pos_samples in	
		let plen = List.length params in
		let params = remove_nth params (List.filter (
			fun i -> i < List.length params
			) (fulls@emptys)) in
		let returns = remove_nth returns (Common.map_partial (
			fun i -> if i >= plen then Some (i - plen) else None
			) (fulls@emptys)) in
		let pos_samples = Common.remove_duplicates pos_samples in
		let _ = List.iter (fun pred -> 
			Format.fprintf Format.std_formatter "Atomic predicate = %a@." Predicate.pprint pred) (params@returns) in
		let separators = try solve_shape_constraints true pos_samples params returns with _ -> [] in
		let separators = 
			Common.map_partial (fun sep -> match sep with
				| Predicate.Forall ([], sep) when 
					List.length (Common.remove_duplicates (Predicate.vars sep)) = 1 -> 
					if TheoremProver.unsat (Predicate.Not sep) then None
					else Some ([], sep)
				| _ -> Some ([], sep)
				) separators
			(*List.map (fun sep -> ([], sep) ) separators*) in
		(* Begin commment old CDNF stuff *)
		(*let predicates = remove_nth predicates (fulls@emptys) in
		let pos_samples = List.map (fun pos_sample -> 
			remove_nth pos_sample (fulls@emptys)) pos_samples in	
		(************************For Learning***************************)
		let pos_samples = Common.remove_duplicates pos_samples in
		let _ = Format.fprintf Format.std_formatter "pred size = %d@." (List.length predicates) in
		let _ = List.iter (fun pred -> 
			Format.fprintf Format.std_formatter "pred = %a@." Predicate.pprint pred) predicates in
		let pos_samples = List.map (fun pos_sample -> 
			Array.of_list (false :: (List.map (fun v -> v = 1) pos_sample))
			) pos_samples in
		(*let neg_samples = List.map (fun neg_sample -> 
			Array.of_list (false :: (List.map (fun v -> v = 1) neg_sample))
			) neg_samples in*)
		(* membership query and equivalence query are answered by tests -- pos_samples *)
		let separator = Cdnflearn.learn 
			(List.length predicates)
			(is_member pos_samples) 
			(is_equivalent predicates pos_samples) in
		let separator = BoolFormula.from_boolformula_t separator in
		let separator = interpret_cdnf predicates separator in *)
		(* End commment old CDNF stuff *)
		
		(* New code: *)
		if (separators = []) then 
			(if flag then List.map (fun sep -> ([], sep)) fullseps else []) @ simpleinv
		else 
			separators @ (if flag then List.map (fun sep -> ([], sep)) fullseps else []) @ simpleinv
		(* Begin commment old CDNF stuff *)
		(*let _ = Format.fprintf Format.std_formatter "separator = %a@." Predicate.pprint separator in 
		if (separator = Predicate.True) then 
			(if flag then List.map (fun sep -> ([], sep)) fullseps else []) @ simpleinv
			(*Format.fprintf Format.std_formatter "Go to template learning ... @."; 
			invmine_learn 1 positives neg_samples tbl enforces env fr*) 
		else 
			(List.map (fun sep -> ([], sep)) (Predicate.split separator))
			@ (if flag then List.map (fun sep -> ([], sep)) fullseps else []) @ simpleinv*)
		(* End commment old CDNF stuff *)
		
let cdnf_learn ni path atomics assertions terminations 
	pos_samples 
	neg_samples 
	tbl enforces env fr measures = 
	if !(Clflags.reachability) then []
	else if not (!Backwalker.data_structure_dealing_flag) then 	
		let pos_samples = List.map (fun pos_samples -> 
			List.filter (fun (k, v) -> Hashtbl.mem tbl k) pos_samples) pos_samples in
		let pos_samples = Common.remove_duplicates pos_samples in	
		if (enforces <> [] && Frame.get_refinement_variable fr = None) then []
		else fast_cdnf_learn ni true path atomics assertions terminations pos_samples neg_samples tbl enforces env fr	[]
	else 
		let measures = (*List is in the program*)
			if (Hashtbl.fold (fun name v res -> 
				if (res) then res else (String.length name > 2 && String.compare (Str.last_chars name 2) "_l" = 0)
				) tbl false) then (Path.mk_ident "l")::measures 
			else measures in
		List.fold_left (fun res measure -> 
			(*let _ = Format.fprintf Format.std_formatter "measure = %s@." (Path.name measure) in
			let _ = Hashtbl.iter (fun k v -> Format.fprintf Format.std_formatter "tbl = %s@." k) tbl in*)
			(** Learn invariant focused on one measure each; tbl is a full table *)
			let mtbl = Hashtbl.create 3 in
			let ho = ["_0"; "_1"; "_2"; "_3"; "_4"; "_r"] in
			(** 1. Cut off fields for the other measures and higher-order inputs and outputs *)
			let enforces = List.filter (fun e -> is_measure_variable e measure) enforces in	
			let _ = Hashtbl.iter (fun name v -> 
				if (List.exists (fun measure -> 
						is_measure_variable name measure 
						(*Common.str_contains name ("_"^(Path.name measure))*)) measures) &&
					not (is_measure_variable name measure) 
					(*Common.str_contains name ("_"^(Path.name measure))*) then ()            (* other measure out *)
				else if (List.exists (fun ho -> Common.str_contains name ho) ho)        (* ho in- and out-put out *)
				then () else Hashtbl.replace mtbl name v) tbl in
			(** 2. Learn equality *)
			if (Hashtbl.length mtbl = 0) then res 
			(* if value is only 0/1, then don't learn (boolean measure) *)
			else if (Hashtbl.fold (fun k _ res -> 
				if (res || (List.for_all (fun measure -> 
						not (is_measure_variable k measure)) measures)) then res 
				else if (List.exists (fun s -> 
					List.assoc k s > 1 || List.assoc k s < 0) pos_samples) then true
				else false  
				) mtbl false) 
			then
				let pos_samples = List.map (fun pos_samples -> 
					List.filter (fun (k, v) -> Hashtbl.mem mtbl k) pos_samples) pos_samples in
				let pos_samples = Common.remove_duplicates pos_samples in	
				let simpleinv = (invmine_learn 0 pos_samples neg_samples mtbl enforces env fr) in
				(*Heuristic: a learning algorithm tries only simply account for equlities invariants at first *)
				if (List.length simpleinv > 0 && !counter = 1) then
					res @ simpleinv
				(** 3. Learn inequality *)	
				else
					let simpleinv = simpleinv @ (invmine_learn 1 pos_samples neg_samples mtbl enforces env fr) in			
					(** 4. Cut off the fields that is not a measure *)
					let _ = Hashtbl.iter (fun name v -> 
						if is_measure_variable name measure 
							(*Common.str_contains name (Path.name measure)*) (* only *measure* left *)
						then () else Hashtbl.remove mtbl name
						) tbl in
					(** 5. Learn complex invariants using CDNF *) 
					let pos_samples = List.map (fun pos_samples -> 
						List.filter (fun (k, v) -> Hashtbl.mem mtbl k) pos_samples) pos_samples in
					let pos_samples = Common.remove_duplicates pos_samples in	
					let cdnfinv = fast_cdnf_learn ni false path atomics assertions terminations 
																							pos_samples neg_samples mtbl enforces env fr measures in
					(** 6. Combine equality and inequality invariants with CDNF invariants *)
					res @ simpleinv @ cdnfinv
			else (res)
		) [] measures
	
	
(******************************************************************
 * Learning minimal invariant in terms of a given set of predicates
 * Type of samples : (string, int) list
 * Type of predicates : (Predicate.t) list
 * Type of enforce_set : string list
 *******************************************************************)
let cbs_learn fpath templates pos_samples neg_samples tbl enforces env fr = 
	(*let pos_samples = [List.hd pos_samples] in
	let neg_samples = [List.hd neg_samples] in*)
	(*let _ = List.iter (fun p ->
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
	(** Heuristic: Equality with more than 2 variables are introduced explicitly *)	
	let equalities = Invmine.learn 0 pos_samples [] tbl enforces env fr in
	let equalities = Common.map_partial (fun (pred) -> match pred with
		| Predicate.Atom (x, Predicate.Le, y) when 
			(List.length (Common.remove_duplicates (Predicate.vars pred)) > 2 ||
					List.length (Common.remove_duplicates (Predicate.vars pred)) > 0) -> 
			Some (Predicate.Atom (x, Predicate.Eq, y))
		| _ -> None
		) equalities in
	let equalities = if equalities = [] then [] else [List.hd equalities] in
	(** names are better with paths in the favor of the implementation *)
	(*let tbl = Hashtbl.create 7 in
	let _ = bind_name_to_path names tbl in*)
	let pos_samples = transl_samples pos_samples tbl in
	let neg_samples = transl_samples neg_samples tbl in
	
	let predicates = equalities @ (genPredicates fpath tbl templates) in
	let pos_samples = List.map (eval_sample predicates) pos_samples in
	let neg_samples = List.map (eval_sample predicates) neg_samples in
	(** we May need to use association rule mining to find container invariants *)
	(*let container_invs = Rulemine.mine_template pos_samples predicates env fr (*tbl*) in
	let _ = Format.fprintf Format.std_formatter "The container invariant is of size: %d@." 
		(List.length container_invs) in
	let _ = List.iter (fun (inv) -> (
		Format.fprintf Format.std_formatter "%a@." Predicate.pprint inv(*;
		Format.fprintf Format.std_formatter "The rule's support is %f@." support;
		Format.fprintf Format.std_formatter "The rule's confidence is %d@." confidence*))
	) container_invs in*)
	(*let _ = List.iter (fun predicate ->
		Predicate.pprint Format.std_formatter predicate
		) predicates in*)
	(*let _ = Format.fprintf Format.std_formatter "\n---\n" in
	let _ = List.iter (fun p ->
		List.iter (fun v ->
		Format.fprintf Format.std_formatter "v : %d " v
		) p) pos_samples in
	let _ = Format.fprintf Format.std_formatter "\n--\n" in
	let _ = List.iter (fun n ->
		List.iter (fun v ->
		Format.fprintf Format.std_formatter "v : %d " v
		) n) neg_samples in
	let _ = Format.fprintf Format.std_formatter "\n" in*)
	(** Before preceding to learning we infer the common predicates *)
	let commons = ref [] in
	let _ = 
		for iter = 0 to ((List.length predicates)-1) do
			match (List.fold_left (fun res sample -> match res with
				| Some v ->
					let v' = List.nth sample iter in
					if (v = v') then Some v else None
				| None -> None 	
			) (Some (List.nth (List.hd pos_samples) iter)) (pos_samples@neg_samples)) with
				| Some v -> 
					if (v = 1) then (commons:=(iter,(List.nth predicates iter))::(!commons)) 
					else (commons:=(iter,(Predicate.Not (List.nth predicates iter)))::(!commons))
				| None -> ()
			done;
	in
	let (common_indices, commons) = List.split (!commons) in
	(** Before go to the constraint solving system, remove commons from both predicates, pos and neg_samples *)
	let predicates = remove_nth predicates common_indices in
	let pos_samples = List.map (fun pos_sample -> remove_nth pos_sample common_indices) pos_samples in
	let neg_samples = List.map (fun neg_sample -> remove_nth neg_sample common_indices) neg_samples in
	(** Go to the constraint solving system *)
	let cvars = Misc.mapi (fun _ i -> Path.mk_ident ("x_" ^ (string_of_int i))) predicates in
	let cvar_bounds = List.map (fun cvar -> 
		Predicate.Or
			(Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 1),
			Atom (Predicate.Var cvar, Predicate.Eq, Predicate.PInt 0))
		) cvars in
	let constraints = 
		List.flatten (List.map (fun pos_sample -> 
			List.map (fun neg_sample -> build_constraint pos_sample neg_sample cvars) neg_samples
			) pos_samples) in
	(*let _ = List.iter (fun predicate ->
		Predicate.pprint Format.std_formatter predicate
		) constraints in *)
	let enforcements = List.fold_left2 (fun res cvar predicate ->
		let vars = Common.remove_duplicates (Predicate.vars predicate) in
		if (List.exists (fun var -> 
			List.exists (fun enforce ->
				if (Hashtbl.mem tbl enforce) then 
					Path.same var (Hashtbl.find tbl enforce)
				else false) enforces) vars) then 
			res @ [Predicate.Atom (Predicate.Var cvar, Predicate.Eq,Predicate.PInt 1)]
		else res
		) [] cvars predicates in
	let enforcements = if (List.length enforcements = 0) then (Predicate.True) else Predicate.big_or enforcements in 
	(*let _ = Format.fprintf Format.std_formatter "enforcements = %a@." Predicate.pprint enforcements in*)
	let constraint_f = Predicate.big_and (cvar_bounds @ [enforcements] @ constraints) in
	let solutions = Bstats.time "rec_solve" (rec_solve constraint_f) cvars in
	(*let _ = List.iter (fun i -> Format.fprintf Format.std_formatter "solution i = %d\n" i) solution in*)
	let separators = List.map (fun solution ->
		interprete solution predicates pos_samples neg_samples 
	) solutions in
	let _ = List.iter (fun separator -> 
		Format.fprintf Format.std_formatter "Cbs separator = %a@." (Predicate.pprint') separator	
	) separators in
	(*let separators = if (separators = []) then equalities else separators in*)
	(*let separator = List.hd separators in*)
	if (List.length enforces > 0) then (*(!commons, separators)*) List.map (fun sep -> (commons, sep)) separators
	(* Fixme? Not a modular programming. Precondition do not use commons while postcondition should. *)
	else (*([], separators)*) List.map (fun sep -> (commons, sep)) separators
	
	
(**********************************
 ***** Preparation of Samples *****
 *********************************)

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
		if (List.length hos = 0) then [(None, plains)]
		else 
			let groups = 
				List.map (fun ho -> 
					(Some ho, ho :: (plains))
					) (hos) in
		 	groups

(** Variables that are not mentioned in neg_samples should be eliminated! *)			
let split_pre_post maximum_attris goods enforcements =
	if (List.length goods = 0) then assert false
	else
		let (locals, _) = List.find (
			fun (ls, _) -> List.length ls >= maximum_attris) goods in
		let post = Common.map_partial (fun local ->
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			(*if (Common.str_contains name_name Instrument.tempt_arr_prefix) 
			then None else*) Some name_name
			) locals in
		let pre = Common.map_partial (fun local ->
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			if (String.compare name_name return_repr = 0) then None
			else if (List.exists (fun e -> String.compare name_name e = 0) enforcements) then None
			else if (Common.str_contains name_name Instrument.tempt_arr_postfix)
			then None else Some name_name
			) locals in 
		let post = Common.remove_duplicates post in
		let pre = Common.remove_duplicates pre in
		[post; pre]

(* split a set of given names based on higher order function *)	
let split_names	names goods = 
	let groups = split_large_goods goods in
	List.map (fun (hf, ns) -> match hf with
		| Some hf -> 
			let hf' = List.hd ns in
			let plains = List.tl ns in
			let _ = assert (String.compare hf hf' = 0) in
			(Some hf, List.filter (fun name -> 
				Common.str_contains name (hf^"_") ||
					List.exists (fun plain -> String.compare plain name = 0) plains	
			) names)
		| None -> 
			let plains = ns in
			(None, List.filter (fun name -> List.exists (fun plain -> String.compare plain name = 0) plains) names)
	) groups
	
	
(************ Learning Linkability and Reachability **************)	
(** Extract Heap samples from pos_samples *)
(*let goods = Hashtbl.fold (fun _ (name, dumpings) res -> 
	if (String.compare name fname = 0) then
		res @ dumpings
	else res
	) pos_samples [] in*)
(** Input a sample set as (string, string * (string list * string list) list) Hashtbl.t *)
(** Return a sample set as 
	(xs:string, ((cons:string, 0:int), (u:int, v:int) list) Hashtbl.t) Hashtbl.t *)
let dump_heaps locals names =
	let result = Hashtbl.create 3 in	
	(List.fold_left (fun res local -> 
		let segments = Str.split (Str.regexp ":") local in
		let n = List.nth segments 0 in
		(*let _ = Format.fprintf Format.std_formatter "local = %s@." local in*)
		if (Common.str_contains n "_heap") then
			(* n is being intersted on heap; log its shape structure into result *)	
			(* drop n from names *)
			let res = List.filter (fun name -> String.compare name n <> 0) res in
			if (List.length segments >= 2) then
				let v = List.nth segments 1 in
				(* Now let us traverse all the links of n*)
				(* 1. extract all the links of n *)
				(let links = Str.split (Str.regexp ",") v in
				let links = List.map (fun link -> 
					let sub_segments = Str.split (Str.regexp "#") link in
					let (cons, index, nodes) = 
						List.nth sub_segments 0, int_of_string (List.nth sub_segments 1), List.nth sub_segments 2 in
					let nodes = Str.split (Str.regexp ";") nodes in 
					let nodes = (int_of_string (List.nth nodes 0), int_of_string (List.nth nodes 1)) in
					(cons, index, nodes)
				) links in
				(* 2. Now can save the structure of n into result *)
				List.iter (fun (cons, index, nodes) -> 
					if (Hashtbl.mem result n) then
						let cache = Hashtbl.find result n in
						if (Hashtbl.mem cache (cons, index)) then
							let cachelinks = Hashtbl.find cache (cons, index) in
							Hashtbl.replace cache (cons, index) (nodes::cachelinks)	
						else 
							Hashtbl.replace cache (cons, index) [nodes]
					else 
						let cache = Hashtbl.create 7 in
						let _ = Hashtbl.replace cache (cons, index) [nodes] in
						Hashtbl.replace result n cache					
				) links; res)	
			else res
		else res	
	) names locals,
	result)	

let dump_locals outch onheaps locals pos_set = 
	let locals = List.map (fun (n, v) -> 
		if (String.contains v '#') then
			let sub_locals = Str.split (Str.regexp ",") v in
			let sub_locals = List.map (fun sub_local -> 
				let sub_name = Str.split (Str.regexp "#") sub_local in
				(List.hd sub_name, List.nth sub_name 1)
				) sub_locals in
			if (List.exists (fun (k, v) -> 
				(let n = 
					if (Common.str_contains n Instrument.tempt_arr_prefix) then
						String.sub n (String.length Instrument.tempt_arr_prefix)
							(String.length n - String.length Instrument.tempt_arr_prefix)
					else if (Common.str_contains n Instrument.tempt_arr_postfix) then
						String.sub n (String.length Instrument.tempt_arr_postfix) 
							(String.length n - String.length Instrument.tempt_arr_postfix)
					else n in
				String.compare k (n^"_r") = 0)
			) sub_locals) then (* complete record *) sub_locals
			else (* missing value. remove this record *) (*assert false	*)sub_locals
		else [(n, v)]
		) locals in
	let locals = List.flatten locals in
	let namevalues = List.fold_left (fun res (name_name, name_value) -> 
		(** the real dump of values *)
		((try Printf.fprintf outch " %d" (int_of_string name_value) 
		with _ -> 
			(Format.fprintf Format.std_formatter "buggy %s:%s \n" name_name name_value;
			assert false));
		res @ [(name_name, (int_of_string name_value))])
		) [] locals in
	let names = List.map fst namevalues in 
	(Printf.fprintf outch "\n"; (pos_set := (!pos_set) @ [namevalues, onheaps]); names)	
	
let dump_small_goods goods names = 
	(*let _ = List.iter (fun name -> Format.fprintf Format.std_formatter "Will dump name=%s@." name) names in*)
	(*let _ = List.iter (fun (ls, _) -> 
		let _ = List.iter (fun l -> Format.fprintf Format.std_formatter "---good--- = %s@." l) ls in
		Format.fprintf Format.std_formatter "\\\\@." 
		) goods in*)
	
	let outch = open_out ("./good.mat") in
	let set = ref [] in
	match (List.fold_left (fun res (locals, _) -> 
		(let (names, onheaps) = dump_heaps locals names in
		if (names = []) then
			(set := !(set) @ [[], onheaps]; None)
		else
		let tbl = Hashtbl.create 9 in
		let _ = List.iter (fun local -> 
			let name = Str.split (Str.regexp ":") local in
			let (name_name, name_value) = (List.hd name, List.nth name 1) in
			(** This is the only difference to dump_goods! *)
			if (List.exists (fun n -> (String.compare n name_name = 0)) names ) then 
				if (Hashtbl.mem tbl name_name) then
					Hashtbl.replace tbl name_name ((Hashtbl.find tbl name_name) @ [name_value])
				else Hashtbl.replace tbl name_name [name_value]
			) locals in
		let locals = Hashtbl.fold (fun k vs res -> 
			res @ [(List.map (fun v -> (k, v)) vs)]) tbl []	in	
		let locals = Misc.lflap locals in
		let names = List.fold_left (fun res local -> 
			try (let names = (dump_locals outch onheaps local set) in		
			match res with
				| Some res -> if (List.length names > List.length res) then Some names else Some res
				| None -> Some names)
			with _ -> (* This record is removed *) res) res locals in
		match res with
			| Some res -> 
				(match names with
					| Some names -> if (List.length names > List.length res) then Some names else Some res 
					| None -> Some res
					) (*Some res*)
			| None -> names
		)) None goods) with
		| Some names -> 
			(close_out outch; (names, 
				List.partition (fun (ele, _) -> List.length ele = List.length names) (!set)
				(*List.filter (fun ele -> List.length ele = List.length names) (!set)*))) 
			(*(close_out outch; (names, !set))*)
		| _ -> (close_out outch; ([], ((!set), [])))

(** dump bads into bad.mat *)
let dump_bads bads names = 
	if (List.length bads = 0) then raise NoSample
	else
	  try let names = List.fold_left (fun names bad ->
				List.filter (fun name -> 
					List.exists (fun (n, _) -> String.compare name n = 0) bad	
				) names
			) names bads in
		List.map (fun bad -> 
			List.map (fun name -> (name, List.assoc name bad)) names
		) bads with _ -> assert false
	
	(*let onebad = try List.map fst (List.hd bads) with _ -> raise NoSample in
	let _ = List.iter (fun b -> Format.fprintf Format.std_formatter "b=%s@." b) onebad in
	(*let _ = assert (List.length names = List.length onebad) in*)
	let indices = List.map (fun name -> 
		let index = ref (-1) in
		(ignore(List.fold_left (fun i b -> 
			if (String.compare b name = 0) then (index := i);
			(i+1)) 0 onebad);
		if (!index = -1) then 
			(Format.fprintf Format.std_formatter "this %s is unfound@." name; !index)
		else (!index)
		)
		) names in	
	let _ = Format.fprintf Format.std_formatter "Here@." in
	let outch = open_out ("./bad.mat") in
	let _ = (c := 1 + (!c)) in
	let neg_set = List.map (fun bad -> (
		let i = ref 0 in	
		let _ = List.iter (fun (n, i) -> Format.fprintf Format.std_formatter "%s=%d " n i) bad in
		let _ = Format.fprintf Format.std_formatter "@." in
		Common.map_partial (fun name -> 
			let _ = (Format.fprintf Format.std_formatter "name (%s)%s: \n" name (List.nth names !i)) in
			let index = List.nth indices (!i) in
			let _ = i := (!i)+1 in
			if (index <> -1) then
				let _ = (Format.fprintf Format.std_formatter "index %d: \n" index) in
				let (_, value) = List.nth bad index in
				let _ = (Format.fprintf Format.std_formatter "value %d: \n" value) in 
				Some (if (!i = (List.length names - 1)) then
					Printf.fprintf outch ("%d\n") value
				else Printf.fprintf outch ("%d ") value;
				(name, value))
			else None) names	
		)) bads in
	let _ = Format.fprintf Format.std_formatter "Here1@." in
	(close_out outch; neg_set)*)
	
(** dump slopes into w.mat *)
(** Fixme: we firtly use the default slopes *)
let dump_slopes n = ()

let fix_missing_values goods =
	(** If some attributes in a sample is missing do remove the sample *)
	
	let maximum_attris = List.fold_left (fun res (locals,_) -> 
		let locals = Common.remove_duplicates 
			(List.map (fun local -> 
				List.hd (Str.split (Str.regexp ":") local)) locals) in
		let len_attris = List.length locals in
		if (len_attris > res) then len_attris else res	
	) 0 goods in
	(*let _ = Format.fprintf Format.std_formatter "maximum_attris=%d@." maximum_attris in*)
	let (result, drops) = List.partition (fun (locals, _) -> 
		let locals = Common.remove_duplicates 
			(List.map (fun local -> List.hd (Str.split (Str.regexp ":") local)) locals) in
		let res = 
			(List.length (locals) = maximum_attris) in
		(*if (not res) then
			List.iter (fun local ->
			Format.fprintf Format.std_formatter "%s@." local
			) locals;*)
		res
	) goods in
	let result = List.map (fun (locals, x) -> 
		(List.filter (fun local -> 
			List.length (Str.split (Str.regexp ":") local) > 1
		) locals, x)
	) result in
	(*let _ = List.iter (fun (ls, _) -> 
		let _ = List.iter (fun l -> Format.fprintf Format.std_formatter "good = %s@." l) ls in
		Format.fprintf Format.std_formatter "\\\\@." 
		) result in*)
	(maximum_attris, result, drops)
	(*if (List.length result != List.length goods) then assert false
	else goods*)
	
	
(******************************
 **** Array Inv Gen Engine ****
 ******************************)
let is_function_witheff frame = match frame with
	| Frame.Farrow (Some _,_,_,_) ->
		let rec get frame = match frame with
			| Frame.Farrow (Some _, _, f, _) -> get f
			| f -> f in
		List.length (Frame.eff (get frame)) > 0 
	| _ -> assert false

(* For Translating a precondition to postcondition *)
let search_exp target returnpath expr = 
	let rec loop expr = match expr with
		| Predicate.Var maybevar -> 
			if (Path.same maybevar target) 
			then (true, expr, None) else (false, expr, None)
  	| Predicate.Binop (e1, op, e2) -> 
			let (flag1, e1, sign1) = loop e1 in
			let (flag2, e2, sign2) = loop e2 in
			(match (flag1, flag2) with
				| (true, false) when op = Predicate.Plus -> (false, e2, Some Predicate.Plus)
				| (true, false) when op = Predicate.Minus -> 
							(false, Predicate.Binop (Predicate.PInt (0-1), Predicate.Times, e2), Some Predicate.Plus)
				| (false, true) when op = Predicate.Plus -> (false, e1, Some Predicate.Plus)
				| (false, true) when op = Predicate.Minus -> (false, e1, Some Predicate.Minus)
				| _ -> (false, expr, match (sign1, sign2) with 
					| (Some _, Some _) -> assert false
					| (Some _, None) -> sign1
					| (None, Some _) -> sign2
					| _ -> None))
  	| _ -> (false, expr, None) in
	let _ = Format.fprintf Format.std_formatter "target=%s and expr=%a@." 
					(Path.name target) Predicate.pprint_pexpr expr in 
	let (f, restexpr, sign) = loop expr in
	match sign with
		| Some sign when sign = Predicate.Plus -> 
			(Predicate.Binop (Predicate.Var returnpath, Predicate.Minus,restexpr))
		| Some sign when sign = Predicate.Minus -> 
			(Predicate.Binop (restexpr, Predicate.Minus, Predicate.Var returnpath))
		| _ -> 
			if (f) then Predicate.Var returnpath else assert false

(** Only translate a precondition if the function return value is base typed *)
let transel_pre_post env fr iters symbpre container_pre_invs preturn = 
	(* The return variable is not counted as we translate pre to post *)
	let allbindings = Frame.get_fun_bindings env fr in
	let returnfr = List.assoc (Frame.returnpath) allbindings in
	let allbindings = List.rev (List.remove_assoc (Frame.returnpath) allbindings) in
	let returnexprs = ref [] in
	let _ = Predicate.map_pred (fun preturn -> match preturn with
		| Predicate.Atom (Predicate.Var r, Predicate.Eq, returnexpr) 
			when (Path.same r Frame.returnpath)-> 
				(returnexprs := returnexpr::(!returnexprs); preturn)
		| notpreturn -> notpreturn
	) preturn in
	let returnexprs = Common.remove_duplicates (!returnexprs) in
	match returnfr with
		| Frame.Fconstr (x,_,_,_,_) when x = Predef.path_int ->
			(** Translate the precondition into postcondition *)
			let invss = List.map (fun preinv -> 
				match (List.fold_left (fun res (p, fr) -> match res with
					| Some inv -> Some inv
					| None -> (
						if (List.exists (fun var -> Path.same var p) (Predicate.vars preinv) ) then
							let invs = (Common.map_partial (fun returnexpr -> 
								if (List.exists (fun var -> Path.same var p) (Predicate.exp_vars returnexpr)) then
									let _ = Format.fprintf Format.std_formatter "" in
									let exp = search_exp p Frame.returnpath returnexpr in
									let postinv = Predicate.subst exp p preinv in
									(** Substitute the iters with their bounds *)
									let bounds = Common.map_partial (fun iter -> (* find the bounds of the iters *)
										let bounds = ref [] in
										let _ = Predicate.map_pred (fun pred -> match pred with
											| Predicate.Atom (Predicate.Var p, _, b) when (Path.same p iter) -> (bounds := b::(!bounds); pred)
											| Predicate.Atom (b, _, Predicate.Var p) when (Path.same p iter) -> (bounds := b::(!bounds); pred)
											| pred -> pred
										) symbpre in
										let bounds = List.filter (fun b -> 
											let bvars = Predicate.exp_vars b in
											(* no variable in bvars correspond to a variable in iters *)
											List.for_all (fun bvar -> not (List.exists (fun i -> Path.same i bvar) iters)) bvars
											) (!bounds) in
										let bounds = Common.remove_duplicates bounds in
										if (List.length bounds > 0) then
											Some (iter, List.hd bounds)
										else None
									) iters in
									(* The invariant is meaningful iff all iters are replaced by the bounds *)
									let postinv = Predicate.apply_substs bounds postinv in
									if (List.for_all (fun var -> List.for_all (fun iter -> not (Path.same var iter)) iters) (Predicate.vars postinv)) 
									then Some postinv else None
								else None
							) returnexprs) in
							if (List.length invs = 0) then None else Some invs
						else res	
					)
				) None allbindings) with
					| Some post_inv -> post_inv
					| None -> assert false 
			) container_pre_invs in
			List.flatten invss
		| fr -> []	
							
(* Array Invariant Learning Engine 
	 -- Generate array invariant from tests (excluding negative samples?) *)
let gen_inv atomics pos_samples neg_samples invariants se_env env = 
	Hashtbl.iter (fun path {Modelsolver.spre=prebads;Modelsolver.spost=postbads} -> 
		let _ = Format.fprintf Format.std_formatter "Learning array invariant for fun %s@." 
						(Path.name path) in
		if (List.length postbads = 0 && (String.compare (Path.name path) (!Backwalker.main_function) = 0)) then 
			(** Fixme? We want invgen on every function except main *)
			(Format.fprintf Format.std_formatter "%s is always good!@." (Path.name path))
		else
			(*let _ = assert (List.length prebads != 0) in*)
			let fname = Path.name path in
			let ffr = Hashtbl.find se_env.funframebindings path in
			let bad = Hashtbl.find se_env.badbindings path in
			let (prebad, postbad) = (bad.pre, bad.post) in
			let effcons = Hashtbl.find se_env.effectbindings path in
			(*let allbindings = Frame.get_fun_bindings env ffr in*)
			let _ = (Format.fprintf Format.std_formatter "Generate array invariant for %s:@." fname) in
			(** put goods in good.mat *)
			let goods = Hashtbl.fold (fun _ (name, dumpings) res -> 
				if (String.compare name fname = 0) then
					res @ dumpings
				else res
				) pos_samples [] in
			let (maximum_attris, goods, _) = fix_missing_values goods in
			let namess = split_pre_post maximum_attris goods [return_repr] in
			let postnames = List.nth namess 0 in
			let prenames = List.nth namess 1 in
			let (_, container_pre_invs) = Rulemine.mine_template false goods prenames env ffr effcons prebad (*tbl*) in
			let _ = Format.fprintf Format.std_formatter "The container pre invariant is of size: %d@." 
				(List.length container_pre_invs) in
			let _ = List.iter (fun (inv) -> (
				Format.fprintf Format.std_formatter "%a@." Predicate.pprint inv)
			) container_pre_invs in	
			
			(** Generally we can skip learning for postcondtion 
					if prenames are exactly same with postnames and effcons is nothing *)	
			let prepost_eq = (*(List.length prenames = List.length postnames)*) true in
			let effcons_nothing = (
				not (is_function_witheff ffr)
				(*let n = ref 0 in (* the number of times that predicate show up in effcons *)
				let _ = ignore(Predicate.map_pred (fun pred' -> match pred' with
					| Predicate.Atom (_,Predicate.Eq,Predicate.FunApp ("UF", _)) -> (n:=1+(!n); pred')
					| _ -> pred') effcons) in	
				(!n = 0)*)
			) in 
			(* The return variable is not counted since we translate pre to post *)
			let postnames = List.filter (fun name -> not (String.compare name (Path.name Frame.returnpath) = 0)) postnames in
			let (iters, container_post_invs) = 
				if (prepost_eq && effcons_nothing) then ([], [])
				else Rulemine.mine_template true goods postnames env ffr effcons postbad (*tbl*) in
			(** Augment the postcondition with precondition by substituting parameter variables with return variable *)	
			let preturn = Hashtbl.find se_env.returnbindings path in
			let container_post_invs = (* Precondition can be transformed into postcondition if only if effcons_nothing not set *)
				if (effcons_nothing) then container_post_invs
				else (transel_pre_post env ffr iters prebad container_pre_invs preturn) @ container_post_invs in	
			let _ = Format.fprintf Format.std_formatter "The container post invariant is of size: %d@." 
				(List.length container_post_invs) in
			let _ = List.iter (fun (inv) -> (
				Format.fprintf Format.std_formatter "%a@." Predicate.pprint inv)
			) container_post_invs in
			if (Hashtbl.mem invariants path) then assert false
				(*(Hashtbl.replace invariants path (invs@(Hashtbl.find invariants path)))*)
			else (*Hashtbl.replace invariants path invs*)
				Hashtbl.replace invariants path
					{ipre = List.map (fun iv -> (false, Hashtbl.create 0, ([], iv))) container_pre_invs; 
					ipost = List.map (fun iv -> (false, Hashtbl.create 0, ([], iv))) container_post_invs}
	) neg_samples
	
(* Simple Learning Engine:
 		-- An invariant for a simple non-recursive function may be directly derivable from program text *)
let simpl_inv_cache = Hashtbl.create 3
let simple_learn se_env env path =
	let _ = Format.fprintf Format.std_formatter "Processing simple predicate for function %s@." (Path.name path) in
	let recflag = fst (Hashtbl.find se_env.funbindings path) in
	if recflag || !(Clflags.reachability) then []
	else if (Hashtbl.mem simpl_inv_cache path) then Hashtbl.find simpl_inv_cache path
	else if (Hashtbl.mem se_env.returnbindings path) then
		let return = Hashtbl.find se_env.returnbindings path in
		let returns = List.rev (Predicate.split return) in
		let (asserts, return) = (List.tl returns, List.hd returns) in
		let asserts = Common.map_partial (fun a -> 
			let vars = Predicate.vars a in
			let fr = Hashtbl.find se_env.funframebindings path in
			let (names, _) = List.split (Frame.get_fun_bindings env fr) in
			if (List.for_all (fun var -> List.exists (fun name -> Path.same var name) names) vars) then
				let tbl = Hashtbl.create 7 in
				let _ = List.iter (fun name -> Hashtbl.replace tbl (Path.name name) name) names in
				let q = (false, tbl, ([], a)) in Some q
			else None
		) asserts in
		let _ = Format.fprintf Format.std_formatter "Processing simple predicate: %a@." Predicate.pprint return in
		let isvalid = ref true in
		let _ = Predicate.map_pred (fun pred -> match pred with
			| Predicate.True -> ((isvalid := false); pred)
			| Predicate.Not Predicate.True -> ((isvalid := false); pred)
			| Predicate.Atom (Predicate.Var p, Predicate.Eq, exp) when Path.same p Frame.returnpath -> 
				let vars = Predicate.exp_vars exp in
				if (List.length vars > 0) then ((isvalid := false); pred)
				else pred
			| pred -> pred
		) return in
		if (!isvalid) then (* set it to a possible invariant *)
			let fr = Hashtbl.find se_env.funframebindings path in
			let (names, _) = List.split (Frame.get_fun_bindings env fr) in
			let tbl = Hashtbl.create 7 in
			let _ = List.iter (fun name -> Hashtbl.replace tbl (Path.name name) name) names in
			let q1 = (false, tbl, ([], return)) in
			(** Consider this predicate may be served in higher order function *)
			let subs = Frame.frame_to_subs fr 0 in
			let q2 = (false, tbl, ([], Predicate.apply_substs subs return)) in
			let q = [q1; q2]@asserts in
			let _ = Hashtbl.replace simpl_inv_cache path q in q
		else
			let q = asserts in
			let _ = Hashtbl.replace simpl_inv_cache path q in q
	else []
	
(**************************************
 ** The Leanring Engine Stub Program **
 **************************************)	
(* extract the least common cloumns *)
let find_least_commons sets = 
	(** control the names as paths so as to be used in predicate *)
	let namess = List.map (fun neg -> fst (List.split neg)) sets in
	let minlen = List.fold_left (fun res names ->
		if (List.length names < res) then List.length names
		else res
	) Pervasives.max_int namess in
	let namess = List.filter (fun names -> List.length names = minlen) namess in
	List.hd namess

(** use algnumber to specify learning algorithm *)
let learn algnumber templates udt_table atomics pos_samples neg_samples invariants se_env env = 
	let _ = (nb_hypo := 0) in
	let n = (counter := !counter + 1; !counter-1) in
	Hashtbl.iter (fun path {Modelsolver.spre=prebads;Modelsolver.spost=postbads} -> 
		let _ = Format.fprintf Format.std_formatter "Learning for fun %d %s@." !counter (Path.name path) in
		if (List.length postbads = 0 && 
				(String.compare (Path.name path) (!Backwalker.main_function) = 0) && 
					not (!(se_env.dty))) then
			(** Find some useful interval-invariants defined in function main *)
			if !(Backwalker.hoflag) then 
				(Format.fprintf Format.std_formatter "%s is always good!@." (Path.name path))
			else
				let atomics = Backwalker.find_atomics (!Backwalker.main_function_path) true in
				let tbl = Hashtbl.create 7 in
				let atomics = List.filter (fun atomic -> (*match atomic with 
					| Predicate.Atom (_,_,Predicate.PInt c)
					| Predicate.Atom (Predicate.PInt c,_,_) ->*)
						let vars = Predicate.vars atomic in
						if (vars <> []) then 
							let _ = List.iter (fun var -> Hashtbl.replace tbl (Path.name var) var) vars in true
						else false
					(*| _ -> false*)) atomics in
				if (atomics <> []) then
					let atomics = List.map (fun atomic -> 
						let _ = Format.fprintf Format.std_formatter "main atomic = %a@." Predicate.pprint atomic in
						(true, tbl, ([], atomic))) atomics in
					Hashtbl.replace invariants (!Backwalker.main_function_path) {ipre = atomics; ipost = []}
				else ()
		else 
			let fname = Path.name path in
			let ffr = Hashtbl.find se_env.funframebindings path in
			let atomics = 
				if (Hashtbl.mem atomics (Path.name path)) then Hashtbl.find atomics (Path.name path) else [] in
			let assertions = 
				if (Hashtbl.mem se_env.assertbindings path) then
					List.filter (fun a -> match a with
						| Predicate.True -> false
						| a when (Predicate.vars a = []) -> false
						| _ -> true) (
					Common.remove_duplicates (Predicate.fullsplit (Hashtbl.find se_env.assertbindings path))) 
				else [] in	
			let terminations = 
				if (Hashtbl.mem se_env.badbindings path) then
					match (Hashtbl.find se_env.badbindings path).pre with 
					| Predicate.Or (Predicate.And (a, _), Predicate.And (Predicate.Not b, _)) when a = b && a <> Predicate.True ->
						Predicate.fullsplit a
					| _ -> []
				else [] in
			(*let bad = Hashtbl.find se_env.badbindings path in
			let (prebad, postbad) = (bad.pre, bad.post) in*)
			let _ = (Format.fprintf Format.std_formatter "Generate invariant for %s:@." fname) in
			(** put goods in good.mat *)
			let goods = Hashtbl.fold (fun _ (name, dumpings) res -> 
				if (String.compare name fname = 0) then
					res @ dumpings
				else res
				) pos_samples [] in
			let (maximum_attris, goods, drops) = fix_missing_values goods in
			if (List.length goods = 0) then (
				(*let simpleinvs = simple_learn se_env env path in
				let _ = Format.fprintf Format.std_formatter "List.length simpleinvs = %d@." (List.length simpleinvs) in
				let _ = List.iter (fun (_, _, (_, inv)) -> 
					Format.fprintf Format.std_formatter "sim = %a@." Predicate.pprint inv) simpleinvs in
				Hashtbl.replace invariants path {ipre = []; ipost = simpleinvs}	*)
			) else	
			(** OLD: Split samples into different clusters depending upon the number of higher order funtions  *)	
			(*let namess = split_large_goods goods in*)
				
			(* NEW: Learning strategy: *)
			(* 1. Dont mention "r" (return) for learning precondition only *)
			(* 2. Mention "r" to enforce a learning for postcondition *)	
			(** generate the enforcements; should be aware that boolean measures do not attend learning  *)
			let measures = Hashtbl.fold (fun _ ms res ->
					List.fold_left (fun res (m, b) -> if b then res @ [m] else res) res ms
				) se_env.measures [] in
			let enforcements = [return_repr; return_repr ^ "_" ^ return_repr; return_repr ^ "_l"; return_repr ^ "_heap"] @
				(List.fold_left (fun res m -> res @ [return_repr ^ "_" ^ (Path.name m)]) [] measures) @
				(List.fold_left (fun res i -> res @ [return_repr ^ "." ^ i]) [] ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"]) in
			let namess = split_pre_post maximum_attris goods enforcements in
			let postnames = List.hd namess in
			let prenamess = List.tl namess in
			(* Note Learning preconditions *)
			let preinvs = List.map (fun names -> 
				if (List.length names = 0) then [] else
				(* Fistly apply rule learning *)
				(*let container_invs = Rulemine.mine_template goods names env ffr prebad (*tbl*) in
				let _ = Format.fprintf Format.std_formatter "The container invariant is of size: %d@." 
					(List.length container_invs) in
				let _ = List.iter (fun (inv) -> (
					Format.fprintf Format.std_formatter "%a@." Predicate.pprint inv(*;
					Format.fprintf Format.std_formatter "The rule's support is %f@." support;
					Format.fprintf Format.std_formatter "The rule's confidence is %d@." confidence*))
				) container_invs in*)
				(** Secondly, put goods in good.mat *)
				let (names, (positives, negatives1)) = dump_small_goods goods names in	
				let (positives, positives_onheap) = (List.map fst positives, positives) in
				let negatives1 = List.map fst negatives1 in
				(** some dropped goods can be added back as names are shrinked *)
				(*let _ = Format.fprintf Format.std_formatter "List.length drops = %d@." (List.length drops) in
				let _ = List.iter (fun name -> Format.fprintf Format.std_formatter "but name = %s@." name) names in*)
				(*let positives = 
					if (List.length drops = 0) then positives
					else positives @ (fst (snd (dump_small_goods drops names))) in*)
				(*let _ = List.iter (fun name -> 
					(Format.fprintf Format.std_formatter "ordering prename: %s\n" name)
					) names in*)
				(** put bads in bad.mat *)
				let negatives2 = try (dump_bads prebads names) with NoSample -> [] in
				(** -----We learn two times; one is for negatives1------ *)
				let invs1 = 
					if (List.length negatives1 > 0) then
						(** control the names as paths so as to be used in predicate *)
						(*let names = fst (List.split (List.hd negatives1)) in*)
						let names = find_least_commons negatives1 in
						(** A useful Optimization: partition names into different higher order functions
						Then we learn each partition separately *)
						let namess = split_names names goods in
						List.fold_left (fun res (hf, names) -> match hf with
							| Some hf when (String.compare hf "r" <> 0 && not (Common.str_contains hf "r.")) 
							(* for precondition check only *) -> 
								let _ = Format.fprintf Format.std_formatter 
												"---- Beging Learning for a precondition check for %s----@." hf in			
								let positives = List.map (fun posset -> 
									List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) posset	
								) positives in
								let negatives1 = Common.map_partial (fun negset -> 
									if (List.exists (fun (s, _) -> Common.str_contains s (hf^"_r")) negset) then None
										(* hf_r is still in, need to filter it out *)
									else Some (List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) negset)
								) negatives1 in
								if (List.length negatives1 = 0) then res
								else 
									let tbl = Hashtbl.create 7 in
									let _ = bind_name_to_path names tbl in
									let invs = 
										if not (List.exists (fun name -> (*Fixme for thid ugly code*)
											String.compare name (hf^"_0") = 0 || String.compare name (hf^"_1") = 0 || String.compare name (hf^"_2") = 0
											|| String.compare name (hf^"_3") = 0 || String.compare name (hf^"_4") = 0) names) then 
											(*hf_0 not in name => we only do precondition check for higher order function*) []
										else if (algnumber=1) then invmine_learn n positives negatives1 tbl [] env ffr
										else if (algnumber=2) then cbs_learn path templates positives negatives1 tbl [] env ffr 
										else if (algnumber=3) then 
											cdnf_learn n path atomics assertions terminations positives negatives1 tbl [] env ffr measures @
											heap_cdnf_learn path positives_onheap tbl [] env ffr udt_table
										else assert false in
									res @ (List.map (fun inv -> (hf, (false, tbl, inv))) invs)
							(*| None -> assert false*)
							| _ -> res
						) [] namess
					else [] in
				(** -----The other is for negatives2 ------ *)
				let invs2 = 
					if (List.length negatives2 > 0) then
						(** control the names as paths so as to be used in predicate *)
						let names = fst (List.split (List.hd negatives2)) in
						let positives = List.map (fun posset -> 
							List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) posset	
						) positives in
						let positives = 
							if (List.length drops = 0) then positives
							else 
								(* Any error indicates we cannot add the drops back *)
								let addbacks = try fst (snd (dump_small_goods drops names)) with _ -> [] in
								let addbacks = List.map fst addbacks in
								let addbacks = List.filter (fun addback -> List.length addback = List.length names) addbacks in 
								positives @ (addbacks) in	
						let tbl = Hashtbl.create 7 in
						let _ = bind_name_to_path names tbl in
						let invs = 
							if (algnumber=1) then invmine_learn n positives negatives2 tbl [] env ffr
							else if (algnumber=2) then 
								(* We should conjoin invs1 with invs2 *)
								let seps = cbs_learn path templates positives negatives2 tbl [] env ffr in
								seps
							else if (algnumber=3) then 
								cdnf_learn n path atomics assertions terminations positives negatives2 tbl [] env ffr measures @
								heap_cdnf_learn path positives_onheap tbl [] env ffr udt_table
							else assert false in
						List.map (fun inv -> (true, tbl, inv)) invs
					else (
						if (algnumber = 1 || algnumber = 3) then 
							let fullnames = names in
							let names = List.filter (fun name -> List.exists (fun m -> 
								Common.str_contains name (Path.name m)
								) measures) names in
							if (List.length names > 0 || algnumber = 3) then
								let fullpositives = positives in
								let positives = List.map (fun posset -> 
									List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) posset	
								) positives in
								let tbl = Hashtbl.create 7 in
								let _ = bind_name_to_path names tbl in
								let fulltbl = Hashtbl.create 7 in
								let _ = bind_name_to_path fullnames fulltbl in
								let _ = Hashtbl.iter (fun k v -> if (Hashtbl.mem fulltbl k) then Hashtbl.replace fulltbl k v) tbl in
								let invs = 
									if algnumber = 1 then (invmine_learn n positives [] tbl) [] env ffr 
									else if algnumber = 3 then 
										cdnf_learn n path atomics assertions terminations fullpositives [] fulltbl [] env ffr measures @
										heap_cdnf_learn path positives_onheap fulltbl [] env ffr udt_table
									else assert false in
								List.map (fun inv -> (true, fulltbl, inv)) invs
							else []
						else []	
					) in
				(*let	invariant = read_invariant names in*)
				(*if (Hashtbl.mem invariants path) then
					(Hashtbl.replace invariants path ((tbl, invariant)::(Hashtbl.find invariants path)))
				else Hashtbl.replace invariants path [(tbl, invariant)]*)
				(*if (Hashtbl.mem invariants path) then
					(Hashtbl.replace invariants path (invs@(Hashtbl.find invariants path)))
				else Hashtbl.replace invariants path invs*)
				(List.map snd invs1) @ invs2
				) prenamess in
			let preinvs = List.flatten preinvs in
			(* Leanring postconditions *)
			(* Fistly apply rule learning *)
			(*let container_invs = Rulemine.mine_template goods postnames env ffr postbad (*tbl*) in
			let _ = Format.fprintf Format.std_formatter "The container invariant is of size: %d@." 
				(List.length container_invs) in
			let _ = List.iter (fun (inv) -> (
				Format.fprintf Format.std_formatter "%a@." Predicate.pprint inv(*;
				Format.fprintf Format.std_formatter "The rule's support is %f@." support;
				Format.fprintf Format.std_formatter "The rule's confidence is %d@." confidence*))
			) container_invs in*)
			(** Try to discover invariant for simple functions from program text *)
			let simpleinvs = simple_learn se_env env path in
			(** Secondly, put goods in good.mat *)
			let (names, (positives, _)) = dump_small_goods goods postnames in	
			let (positives, positives_onheap) = List.map fst positives, positives in
			(*let _ = List.iter (fun name -> 
				(Format.fprintf Format.std_formatter "ordering postname: %s\n" name)
				) names in	*)
			(** put the corresponding slopes *)	
			let _ = dump_slopes (List.length names) in 
			(* Fixme? *)
			let postinvs = (*Bstats.time "cbs_learn"*)
				(** put bads in bad.mat *)
				(** Revise: Bad sample cab be empty for post-condition as assertion can be put inside the definition *)
				try 
					let negatives = dump_bads postbads names in
					(** control the names as paths so as to be used in predicate *)
					let names = fst (List.split (List.hd negatives)) in
					let positives = List.map (fun posset -> 
						List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) posset	
					) positives in
					let positives = 
						if (List.length drops = 0) then positives
						else 
							(* Any error indicates we cannot add the drops back *)
							let addbacks = try fst (snd (dump_small_goods drops names)) with _ -> [] in
							let addbacks = List.map fst addbacks in
							let addbacks = List.filter (fun addback -> List.length addback = List.length names) addbacks in 
							positives @ (addbacks) in
					let tbl = Hashtbl.create 7 in
					let _ = bind_name_to_path names tbl in
					let _ = (Format.fprintf Format.std_formatter "post good/bad prepared \n") in
					let invs = 
						if algnumber=1 then
							(invmine_learn n positives negatives tbl) enforcements env ffr 
						else if algnumber=2 then (cbs_learn path templates positives negatives tbl) enforcements env ffr 
						else if algnumber=3 then 
							cdnf_learn n path atomics assertions terminations positives negatives tbl enforcements env ffr measures @
							heap_cdnf_learn path positives_onheap tbl enforcements env ffr udt_table
						else assert false in
					List.map (fun inv -> (true, tbl, inv)) invs
				with NoSample -> (
					if (algnumber=1 || algnumber=3) then
						(* If generating invariant, why keeps names that include non-meansures *)		
						let fullnames = names in
						let names = List.filter (fun name -> Common.str_contains name "_l" || List.exists (fun m -> 
							Common.str_contains name (Path.name m)
							) measures) names in
						if (List.length names > 0 || algnumber = 3) then
								let fullpositives = positives in
								let positives = List.map (fun posset -> 
									List.filter (fun (s, _) -> List.exists (fun name -> String.compare s name = 0) names) posset	
								) positives in
								let tbl = Hashtbl.create 7 in
								let _ = bind_name_to_path names tbl in
								let fulltbl = Hashtbl.create 7 in
								let _ = bind_name_to_path fullnames fulltbl in
								let _ = Hashtbl.iter (fun k v -> if (Hashtbl.mem fulltbl k) then Hashtbl.replace fulltbl k v) tbl in
								let invs = 
									if algnumber=1 then (invmine_learn n positives [] tbl) enforcements env ffr 
									else if algnumber=3 then 
										(cdnf_learn n path atomics assertions terminations fullpositives [] fulltbl enforcements env ffr measures @
										heap_cdnf_learn path positives_onheap fulltbl enforcements env ffr udt_table)
									else assert false in
							List.map (fun inv -> (true, fulltbl, inv)) invs
						else []
					else []) in
			(*let	invariant = read_invariant names in*)
			if (Hashtbl.mem invariants path) then assert false
				(*(Hashtbl.replace invariants path (invs@(Hashtbl.find invariants path)))*)
			else (*Hashtbl.replace invariants path invs*)
				let _ = List.iter (fun (_,_,(_, inv)) -> 
					Format.fprintf Format.std_formatter "Infer an invaraint from pre  %a@." Predicate.pprint inv
					) (preinvs) in
				let _ = List.iter (fun (_,_,(_, inv)) -> 
					Format.fprintf Format.std_formatter "Infer an invaraint from post  %a@." Predicate.pprint inv
					) (postinvs) in
				let _ = List.iter (fun (_,_,(_, inv)) -> 
					Format.fprintf Format.std_formatter "Infer an invaraint from simple  %a@." Predicate.pprint inv
					) (simpleinvs) in		
				Hashtbl.replace invariants path {ipre = preinvs; ipost = postinvs@simpleinvs}
		) neg_samples	
