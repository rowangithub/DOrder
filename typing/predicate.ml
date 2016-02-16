open Parsetree
open Asttypes
open Types
open Format

module C = Common

type binop =
    Plus
  | Minus
  | Times
  | Div
	| Mod

type binrel =
    Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le 

type patpexpr =
    PPInt of int list
  | PVar of Path.t list
  | PFunApp of Longident.t * patpexpr list 
  | PBinop of patpexpr * binop list * patpexpr
  | PField of string * patpexpr 
  | PProj of int * patpexpr

type tpat =
    PTrue
  | PAtom of patpexpr * binrel list * patpexpr
  | PIff of patpexpr * tpat
  | PNot of tpat
  | PAnd of tpat * tpat
  | POr of tpat * tpat
	| PReach of patpexpr * patpexpr
	| PLink of patpexpr * string * int * patpexpr * patpexpr
	| PForall of (Path.t list) * tpat

type pexpr =
    PInt of int 
  | Var of Path.t
  | FunApp of string * pexpr list  
  | Binop of pexpr * binop * pexpr
  | Field of string * pexpr
  | Proj of int * pexpr
	
type t =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of pexpr * t
  | Not of t
  | And of t * t 
  | Or of t * t
	| Reach of pexpr * pexpr
	| Link of pexpr * string * int * pexpr * pexpr
	| Forall of (Path.t list) * t
	| Bool of pexpr

let pprint_op = function
	| Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
	| Mod -> "%"

let pprint_rel = function
    Eq -> "="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let rec pprint_pexpr' ppf = function
  | PInt n ->
      if n < 0 then fprintf ppf "(0 - %d)" (-n)
      else fprintf ppf "%d" n
  | Var x ->
      fprintf ppf "%s" (Path.unique_name x)
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" f (Common.pprint_list " " pprint_pexpr') pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
				| Mod -> "%"
      in fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr' p opstr pprint_pexpr' q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr' pexp f
  | Proj (n, pexp) ->
      fprintf ppf "@[%a.%d@]" pprint_pexpr' pexp n

let rec pprint' ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr' p (pprint_rel rel) pprint_pexpr' q
  | Iff (px, q) ->
      fprintf ppf "@[(%a@ <=>@;<1 2>%a)@]" pprint_pexpr' px pprint' q
  | Not p ->
      fprintf ppf "@[(-.@ %a)@]" pprint' p
  | And (p, q) ->
      fprintf ppf "@[(%a@ and@;<1 2>@;<1 2>%a)@]" flatten_conjuncts' p flatten_conjuncts' q
  | Or (p, q) ->
      fprintf ppf "@[(%a@ or@;<1 2>@;<1 2>%a)@]" flatten_disjuncts' p flatten_disjuncts' q
	| Reach (d, u) -> 
			fprintf ppf "@[(%s@ (%a, %a))@]" "reach" pprint_pexpr' d pprint_pexpr' u
	| Link (d, c, f, u, v)	->
			fprintf ppf "@[%s@ (%a, %s, %d, %a, %a)@]" 
				"link" pprint_pexpr' d c f pprint_pexpr' u pprint_pexpr' v	
	| Forall (ps, p) -> 
			fprintf ppf "@[forall (%s). %a@]" 
				(List.fold_left (fun res p -> 
					res ^ (Path.unique_name p) ^ " ") "" ps) pprint' p 
	| Bool b -> fprintf ppf "@[%a@]" pprint_pexpr' b
and flatten_conjuncts' ppf = function
  | And (And (p1, p2), And (q1, q2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@;<1 0>%a@]"
        flatten_conjuncts' p1 flatten_conjuncts' p2
        flatten_conjuncts' q1 flatten_conjuncts' q2
  | And (And (p1, p2), q)
  | And (q, And (p1, p2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@]"
        pprint' q flatten_conjuncts' p1 flatten_conjuncts' p2
  | p -> pprint' ppf p
and flatten_disjuncts' ppf = function
  | Or (Or (p1, p2), Or (q1, q2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@;<1 0>%a@]"
        flatten_disjuncts' p1 flatten_disjuncts' p2
        flatten_disjuncts' q1 flatten_disjuncts' q2
  | Or (Or (p1, p2), q)
  | Or (q, Or (p1, p2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@]"
        pprint' q flatten_disjuncts' p1 flatten_disjuncts' p2
  | p -> pprint' ppf p



let rec pprint_pexpr ppf = function
  | PInt n ->
      if n < 0 then fprintf ppf "(0 - %d)" (-n)
      else fprintf ppf "%d" n
  | Var x ->
      fprintf ppf "%s" ((Common.path_name ()) x)
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" f (Common.pprint_list " " pprint_pexpr) pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
				| Mod -> "%"
      in fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p opstr pprint_pexpr q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr pexp f
  | Proj (n, pexp) ->
      fprintf ppf "@[%a.%d@]" pprint_pexpr pexp n

let rec pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p (pprint_rel rel) pprint_pexpr q
  | Iff (px, q) ->
      fprintf ppf "@[(%a@ <=>@;<1 2>%a)@]" pprint_pexpr px pprint q
  | Not p ->
      fprintf ppf "@[(not@ %a)@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[(%a@ and@;<1 2>@;<1 2>%a)@]" flatten_conjuncts p flatten_conjuncts q
  | Or (p, q) ->
      fprintf ppf "@[(%a@ or@;<1 2>@;<1 2>%a)@]" flatten_disjuncts p flatten_disjuncts q
	| Reach (d, u) -> 
			fprintf ppf "@[(%s@ (%a, %a))@]" "reach" pprint_pexpr d pprint_pexpr u
	| Link (d, c, f, u, v)	->
			if (f >= 10) then
				let s, t = (string_of_int (f/10)), (string_of_int (f mod 10)) in
				fprintf ppf "@[%s@ (%a, %s, %s, %s, %a, %a)@]" 
					"link" pprint_pexpr d c s t pprint_pexpr u pprint_pexpr v	
			else
				fprintf ppf "@[%s@ (%a, %s, %d, %a, %a)@]" 
					"link" pprint_pexpr d c f pprint_pexpr u pprint_pexpr v
	| Forall (ps, p) -> 
			fprintf ppf "@[forall (%s). %a@]" 
				(List.fold_left (fun res p -> 
					res ^ (Path.name p) ^ " ") "" ps) pprint p 
	| Bool b -> fprintf ppf "@[%a@]" pprint_pexpr b
and flatten_conjuncts ppf = function
  | And (And (p1, p2), And (q1, q2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@;<1 0>%a@]"
        flatten_conjuncts p1 flatten_conjuncts p2
        flatten_conjuncts q1 flatten_conjuncts q2
  | And (And (p1, p2), q)
  | And (q, And (p1, p2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@]"
        pprint q flatten_conjuncts p1 flatten_conjuncts p2
  | p -> pprint ppf p
and flatten_disjuncts ppf = function
  | Or (Or (p1, p2), Or (q1, q2)) ->
      fprintf ppf "@[%a@ or@;<1 0>%a@ or@;<1 0>%a@ or@;<1 0>%a@]"
        flatten_disjuncts p1 flatten_disjuncts p2
        flatten_disjuncts q1 flatten_disjuncts q2
  | Or (Or (p1, p2), q)
  | Or (q, Or (p1, p2)) ->
      fprintf ppf "@[%a@ or@;<1 0>%a@ or@;<1 0>%a@]"
        pprint q flatten_disjuncts p1 flatten_disjuncts p2
  | p -> pprint ppf p

let equals(p, q) = Atom(p, Eq, q)

let (==.) p q = equals (p, q)

let (!=.) p q = Atom (p, Ne, q)

let (>=.) p q = Atom (p, Ge, q)

let (>.) p q = Atom (p, Gt, q)

let (<=.) p q = Atom (p, Le, q)

let (<.) p q = Atom (p, Lt, q)

let (&&.) p q = And (p, q)

let (||.) p q = Or (p, q)

let (!.) p = Not p

let (<=>.) p q = Iff (p, q)

let (+-) p q = Binop (p, Plus, q)

let ( *-) p q = Binop (p, Times, q)

let (/-) p q = Binop (p, Div, q)

let (--) p q = Binop (p, Minus, q)         

let implies(p, q) = (!. p) ||. q

let (=>.) p q = implies (p, q)

let logic_equals p q = 
	(p =>. q) &&. (q =>. p)

let find_const c =
  match (Env.lookup_constructor (Longident.Lident c) Env.initial).cstr_tag with
    |  Cstr_constant n -> n
    | _ -> assert false

let (int_true, int_false) = (PInt (find_const "true"), PInt (find_const "false"))

let expand_iff = function
  | Iff (px, q) -> ((px ==. int_true) &&. q) ||. ((px ==. int_false) &&. (!. q))
  | _ -> assert false

let big_and = function
  | c :: cs -> List.fold_left (&&.) c cs
  | [] -> True

let big_or = function
  | c :: cs -> List.fold_left (||.) c cs
  | [] -> Not True

let rec pexp_map_vars f pexp =
  let rec map_rec = function
      Var x -> f x
    | FunApp (fn, e) ->
        FunApp (fn, List.map map_rec e)
    | Binop (e1, op, e2) ->
        Binop (map_rec e1, op, map_rec e2)
    | Field (f, pexp) ->
        Field (f, map_rec pexp)
    | Proj (n, pexp) ->
        Proj (n, map_rec pexp)
    | e ->
        e
  in map_rec pexp

let rec map_vars f pred =
  let rec map_rec = function
      True ->
        True
    | Atom (e1, rel, e2) ->
        Atom (pexp_map_vars f e1, rel, pexp_map_vars f e2)
    | Iff (px, q) -> Iff (pexp_map_vars f px, map_rec q)
    | Not p ->
        Not (map_rec p)
    | And (p, q) ->
        And (map_rec p, map_rec q)
    | Or (p, q) ->
        Or (map_rec p, map_rec q)
		| Reach (d, u) ->
				Reach (pexp_map_vars f d, pexp_map_vars f u)
		| Link (d, c, field, u, v)	->
				Link (pexp_map_vars f d, c, field, pexp_map_vars f u, pexp_map_vars f v)
		| Forall (ps, p) -> 
				Forall (ps, map_rec p)
		| Bool b -> Bool (pexp_map_vars f b)
  in map_rec pred
	
let rec map_quantifiers f pred = 
	let rec map_rec pred = match pred with
		| True -> True
		| Atom _ -> pred
		| Iff (px, q) -> Iff (px, map_rec q)
		| Not p -> Not (map_rec p)
		| And (p, q) -> And (map_rec p, map_rec q)
		| Or (p, q) -> Or (map_rec p, map_rec q)
		| Reach (d, u) -> pred
		| Link (d, c, field, u, v) -> pred
		| Forall (ps, p) -> Forall (List.map (fun p -> f p) ps, map_rec p) 
		| Bool b -> pred in
	map_rec pred

let exp_vars_unexp = function
  | PInt _ -> ([], [])
  | Var x -> ([], [x])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) | Proj (_, e) -> ([e], [])

let exp_vars e =
  C.expand exp_vars_unexp [e] []

let var_unexp = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], exp_vars e1 @ exp_vars e2)
  | Iff (e, q) -> ([q], exp_vars e)
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])
	| Reach (d, u) -> ([], exp_vars d @ exp_vars u)
	| Link (d, c, f, u, v)	-> ([], exp_vars d @ exp_vars u @ exp_vars v)
	| Forall (_, p) -> ([p], [])
	| Bool b -> ([], exp_vars b)

let var_quantifier_exp = function
	| True -> ([], [])
  | Atom (e1, _, e2) -> ([], [])
  | Iff (e, q) -> ([q], [])
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])
	| Reach (d, u) -> ([], [])
	| Link (d, c, f, u, v)	-> ([], [])
	| Forall (ps, p) -> ([p], ps)
	| Bool b -> ([], [])

let quantifiers e = 
	C.expand var_quantifier_exp [e] []

(* vars function does not return universall quantified variables *)
let vars e =
  let vars = C.expand var_unexp [e] [] in
	let quantifiers = C.expand var_quantifier_exp [e] [] in
	List.filter (fun var -> 
		List.for_all (fun quantifier -> not (Path.same var quantifier)) quantifiers) vars 
	
(** Find integers but not coeffs *)
let exp_ints_unexp = function
  | PInt i -> ([], [i])
  | Var x -> ([], [])
	| Binop (e1, Times, e2) ->
		let e1vars = exp_vars e1 in
		let e2vars = exp_vars e2 in
		if (e1vars = [] && e2vars = []) then ([e1; e2], [])
		else if (e1vars = []) then ([e2], []) 
		else if (e2vars = []) then ([e1], [])
		else ([e1; e2], [])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) | Proj (_, e) -> ([e], [])

let exp_ints e =
  C.expand exp_ints_unexp [e] []

let int_unexp = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], exp_ints e1 @ exp_ints e2)
  | Iff (e, q) -> ([q], exp_ints e)
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])
	| Reach (d, u) -> ([], exp_ints d @ exp_ints u)
	| Link (d, c, f, u, v)	-> ([], exp_ints d @ exp_ints u @ exp_ints v)
	| Forall (_, p) -> ([p], []) 
	| Bool b -> ([], exp_ints b)

let ints e = C.expand int_unexp [e] []
(** Find integers *)	

(** Find coefficients *)
let exp_coeffs_unexp = function
  | PInt i -> ([], [])
  | Var x -> ([], [])
	| Binop (e1, Times, e2) ->
		let e1vars = exp_vars e1 in
		let e2vars = exp_vars e2 in
		if (e1vars = [] && e2vars = []) then ([], [])
		else if (e1vars = []) then ([e2], exp_ints e1) 
		else if (e2vars = []) then ([e1], exp_ints e2)
		else ([e1; e2], [])
	| Binop (e1, Mod, e2) -> ([e1], [])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) | Proj (_, e) -> ([e], [])

let exp_coeffs e =
  C.expand exp_coeffs_unexp [e] []

let coeff_unexp = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], exp_coeffs e1 @ exp_coeffs e2)
  | Iff (e, q) -> ([q], exp_coeffs e)
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])
	| Reach (d, u) -> ([], exp_coeffs d @ exp_coeffs u)
	| Link (d, c, f, u, v)	-> ([], exp_coeffs d @ exp_coeffs u @ exp_coeffs v)
	| Forall (_, p) -> ([p], [])
	| Bool b -> ([], exp_coeffs b)

let coeffs e = 
	C.expand coeff_unexp [e] []
(** Find coefficients *)

let subst v x pred = map_vars (fun y -> if Path.same x y then v else Var y) pred

let apply_substs subs pred =
	let subs = List.stable_sort (fun (p1, e1) (p2, e2) -> 
		let evars1 = exp_vars e1 in
		let evars2 = exp_vars e2 in
		if (List.exists (fun evar1 -> Path.same evar1 p2) evars1) then (1)
		else if (List.exists (fun evar2 -> Path.same evar2 p1) evars2) then (-1)
		else 0
		) subs in
  let substitute p (x, e) = subst e x p in List.fold_left substitute pred subs
	
let exp_apply_substs subs pexp = 
	let subst v x pexp = pexp_map_vars (fun y -> if Path.same x y then v else Var y) pexp in
	let substitute p (x, e) = subst e x p in List.fold_left substitute pexp subs	

let rec instantiate_named_vars subs pred =
		map_vars (fun y -> Var (List.assoc (Path.ident_name_crash y) subs)
			) pred

let transl_op = function
  | Predexp_plus -> Plus
  | Predexp_minus -> Minus
  | Predexp_times -> Times
  | Predexp_div -> Div
	| Predexp_mod -> Mod
 
let transl_rel = function
  | Pred_eq -> Eq
  | Pred_ne -> Ne
  | Pred_gt -> Gt
  | Pred_ge -> Ge
  | Pred_lt -> Lt
  | Pred_le -> Le


let map_expr f pred = 
	let rec map_rec_pexpr pexpr = 
		(*let pexpr = f pexpr in*)
		let pexpr = match pexpr with
    | FunApp (fn, e) -> (*f pexpr*) FunApp (fn, List.map map_rec_pexpr e)
    | Binop (e1, op, e2) ->
        Binop (map_rec_pexpr e1, op, map_rec_pexpr e2)
    | Field (f, pexp) ->
        Field (f, map_rec_pexpr pexp)
    | Proj (n, pexp) ->
        Proj (n, map_rec_pexpr pexp)
    | e -> e in
		f pexpr in
	let rec map_rec_pred = function
    | True -> True
    | Atom (e1, rel, e2) ->
        Atom (map_rec_pexpr e1, rel, map_rec_pexpr e2)
    | Iff (px, q) -> Iff (map_rec_pexpr px, map_rec_pred q)
    | Not p ->
        Not (map_rec_pred p)
    | And (p, q) ->
        And (map_rec_pred p, map_rec_pred q)
    | Or (p, q) ->
        Or (map_rec_pred p, map_rec_pred q)
		| Reach (d, u) -> Reach (map_rec_pexpr d, map_rec_pexpr u)
		| Link (d, c, f, u, v) -> Link (map_rec_pexpr d, c, f, map_rec_pexpr u, map_rec_pexpr v)
		| Forall (ps, p) -> Forall (ps, map_rec_pred p)
		| Bool b -> Bool (map_rec_pexpr b)
  in map_rec_pred pred
	
let map_expr_from_top f pred = 
	let rec map_rec_pexpr pexpr = 
		let pexpr = f pexpr in
		match pexpr with
    | FunApp (fn, e) -> (*f pexpr*) FunApp (fn, List.map map_rec_pexpr e)
    | Binop (e1, op, e2) ->
        Binop (map_rec_pexpr e1, op, map_rec_pexpr e2)
    | Field (f, pexp) ->
        Field (f, map_rec_pexpr pexp)
    | Proj (n, pexp) ->
        Proj (n, map_rec_pexpr pexp)
    | e -> e in
	let rec map_rec_pred = function
    | True -> True
    | Atom (e1, rel, e2) ->
        Atom (map_rec_pexpr e1, rel, map_rec_pexpr e2)
    | Iff (px, q) -> Iff (map_rec_pexpr px, map_rec_pred q)
    | Not p ->
        Not (map_rec_pred p)
    | And (p, q) ->
        And (map_rec_pred p, map_rec_pred q)
    | Or (p, q) ->
        Or (map_rec_pred p, map_rec_pred q)
		| Reach (d, u) -> Reach (map_rec_pexpr d, map_rec_pexpr u)
		| Link (d, c, f, u, v)	-> Link (map_rec_pexpr d, c, f, map_rec_pexpr u, map_rec_pexpr v)
		| Forall (ps, p) -> Forall (ps, map_rec_pred p)
		| Bool b -> Bool (map_rec_pexpr b)
  in map_rec_pred pred	
	
	
let map_pred f pred = 
	let rec map_rec_pred pred = 
		let pred = f pred in
		match pred with
    | True -> pred
    | Atom (e1, rel, e2) -> pred
    | Iff (px, q) -> Iff (px, map_rec_pred q)
    | Not p ->
        Not (map_rec_pred p)
    | And (p, q) ->
        And (map_rec_pred p, map_rec_pred q)
    | Or (p, q) ->
        Or (map_rec_pred p, map_rec_pred q) 
		| Reach (d, u) -> Reach (d, u)
		| Link (d, c, f, u, v)	-> Link (d, c, f, u, v)
		| Forall (ps, p) -> Forall (ps, map_rec_pred p)	
		| Bool b -> Bool b in
	map_rec_pred pred
	
let map_pred_from_bottom f pred = 
	let rec map_rec_pred pred = 
		let pred = match pred with
    | True -> pred
    | Atom (e1, rel, e2) -> pred
    | Iff (px, q) -> Iff (px, map_rec_pred q)
    | Not p ->
        Not (map_rec_pred p)
    | And (p, q) ->
        And (map_rec_pred p, map_rec_pred q)
    | Or (p, q) ->
        Or (map_rec_pred p, map_rec_pred q)
		| Reach (d, u) -> Reach (d, u)		
		| Link (d, c, f, u, v)	-> Link (d, c, f, u, v)
		| Forall (ps, p) -> Forall (ps, map_rec_pred p)
		| Bool b -> Bool b
		in f pred in
	map_rec_pred pred
	
(* Return all function applications in the pred *)
let get_all_funs pred = 
	let funs = ref [] in
	(ignore (map_expr (fun pexp -> match pexp with
		| FunApp _ -> ((funs := pexp::(!funs)); pexp)
		| _ -> pexp
		) pred); Common.remove_duplicates (!funs))
		
let exp_var expr = match expr with
	| Var var -> var
	| _ -> assert false

(* Decompose the pred into a conjunctions of smaller predicates *)
let rec split p = match p with
	| And (p1, p2) -> (split p1) @ (split p2)  
	| _ -> [p]

(* Decompose the pred into a disjunction of smaller predicates *)
let rec split_or p = match p with
	| Or (p1, p2) -> (split_or p1) @ (split_or p2)  
	| _ -> [p]

(* Fully decompose the pred *)
let rec fullsplit p = match p with
	| True -> [p]
	| Atom (e1, rel, e2) -> [p]
	| Iff (px, q) -> fullsplit q
	| Not p -> [p]
	| And (p, q) ->
	    (fullsplit p) @ (fullsplit q)
	| Or (p, q) ->
	    (fullsplit p) @ (fullsplit q)
	| Reach _ -> [p]		
	| Forall _ -> [p]
	| Link _ -> [p]
	| Bool _ -> [p]
	(*| _ -> assert false*)

(* Instantiate the quantifier with the variables in forall predicates *)
let inst_forall pred variables = pred
	(*map_pred (fun pred -> match pred with
		| Forall (ps, pred) -> 
			let k = 
				if (List.length variables <= List.length ps) then List.length variables
				else List.length ps in
			let varss = Common.extract k variables in
			let varss = List.fold_left (fun res l -> 
				if (List.length l  = 0) then assert false
				else if (List.length l = 1) then res @ [l]
				else if (List.length l = 2 ) then res @ [l; List.rev l]
				else assert false) [] varss in
			let ps = Common.sublist 0 (k-1) ps in
			big_and (List.map (fun vars -> 
				let subs = List.map2 (fun p var -> (p, var)) ps vars in
				apply_substs subs pred
				) varss)
		| pred -> pred
		) pred*)

(*exception Non_fixpoint						
let rec sub_deduct properties pred =
	match pred with
		| Forall (_, And (Or (Not (Link (d', c', f', u', v')), reach),
				Or (Not (_reach), Link (_d, _c, _f, _u, _v)))) ->
			let a_properties = !properties in
			let composition = List.fold_left (fun res (d, c, f, uvar, vvar, property) -> 
				if ( d = _d && String.compare c _c = 0 && f = _f ) then
					match reach with
					| Or (
						And (Atom (x, Eq, u''), Reach (d'', v'')), 
						(*Link*) _) when v' = v'' && u' = u'' ->
						let _ = 
							if (List.mem (d'', c, f, uvar, vvar, property) (!properties)) then ()
							else (properties := (!properties)@
								(Common.map_partial (fun (md, mc, mf, mu, mv, mp) -> 
									if (md = d && String.compare mc c = 0 && mf <> f) then
										Some (d'', mc, mf, mu, mv, mp)
									else None
									) (!properties)) @
								[
								(d'', c, f, uvar, vvar, property);                  (* for further deduction on link predicate *)
								(d'', "", (-1), vvar, vvar, subst x uvar property)  (* for futher deduction on reach predicate *)
								](*; 
								raise Non_fixpoint*)) in	
						(Forall ([vvar], (implies (Reach (d'', Var vvar), subst x uvar property))))::res
					| Not True -> res
					| _ -> assert false
				else res
				) [] a_properties in
			let _ = if (List.length (!properties) > List.length a_properties) then raise Non_fixpoint else () in
			let _ = List.iter (fun d ->
				Format.fprintf Format.std_formatter "A.create a deduction : %a@." pprint' d) composition in
			(*let _ = assert (List.length composition <= 1) in*)
			if (composition = []) then pred
			else big_and (composition @ [pred])
				(*And ((List.hd composition), pred)*)
		| Forall (_, And (Or (Not (Reach (d', u')), Or (Atom (x', Eq, u''), (*Reach*) segments)),
			Or (Not (Or (Atom (_x, Eq, __u), (*Reach*) _)), Reach (_d, _u)))) when (d' = _d && u' = u'' && __u = _u) ->
			let a_properties = !properties in
			let composition = List.fold_left (fun res (d, c, f, uvar, vvar, property) -> 
				if (d = _d && String.compare c "" = 0 && f = (-1) && uvar = vvar) then
					(* updated on April 10th: go over the reachable segments and deduce... *)		
					let _ = List.iter (fun seg -> match seg with
						| Reach (dseg, _) -> 
							if (List.mem (dseg, "", (-1), uvar, vvar, property) (!properties)) then ()
							else
								(properties := (!properties)@[(dseg, "", (-1), uvar, vvar, property)]
								)
							(*Some (Forall ([uvar], implies (Reach (dseg, Var uvar), property)))*)
						| _ -> () ) (fullsplit segments) in
					(subst x' uvar property) :: res
				else res 
				) [] a_properties in
			let _ = if (List.length (!properties) > List.length a_properties) then raise Non_fixpoint else () in	
			let _ = List.iter (fun d ->
				Format.fprintf Format.std_formatter "B.create a deduction : %a@." pprint' d) composition in	
			(*let _ = assert (List.length composition <= 1) in*)
			if (composition = []) then pred
			else big_and (composition @ [pred])	
		| Or (p1, p2) -> 
			let p1s = (split p1) in
			let p2s = (split p2) in
			Or (big_and (*List.map (sub_deduct properties) p1s*) (fix properties p1s), 
			big_and (*List.map (sub_deduct properties) p2s*) (fix properties p2s))
		| pred -> pred		
and fix properties preds =
	try 
		List.map (sub_deduct properties) preds
	with Non_fixpoint -> fix properties preds *)
		
(* Deduction Proof System: a (universally quantified) verification condition *)
(* for data structure is deducted ... ... contact me for the details *)
let deduct_datastructure_proof vc = vc
	(*let preds = split vc in
	let properties = List.fold_left (fun res pred -> 
		match pred with
			| Forall (ps, 
					Or (Not (Link (d, c, f, (Var uvar), (Var vvar))), property)) -> 
					(**  property is not a link or a reach *)
					(match property with
						| Link _ -> res
						| Reach _ -> res
						| property -> (d, c, f, uvar, vvar, property)::res)
			| Forall (ps,
					Or (Not (Reach (d, (Var uvar))), property)) ->
					(** propety is not a link or reach *)
					(match property with
						| Link _ -> res
						| Reach _ -> res
						| property -> (d, "", (-1), uvar, uvar, property)::res)
			| _ -> res) [] preds in
	let properties = ref properties in
	let preds = fix properties preds (*List.map (fun pred -> 
		sub_deduct properties pred
		) preds*) in
	big_and preds*)

let rec is_link pred = 
	match pred with
		| Iff (px, q) -> is_link q
		| Not p -> is_link p
		| And (p, q) ->
	    (is_link p) || (is_link q)
		| Or (p, q) ->
	    (is_link p) || (is_link q)	
		| Forall (_, p) -> is_link p 
		| Link _ -> true
		| Reach _ -> false	
		| Bool _ -> false
		| _ -> false


let rec is_shape_pred pred = 
	match pred with
		| Iff (px, q) -> is_shape_pred q
		| Not p -> is_shape_pred p
		| And (p, q) ->
	    (is_shape_pred p) || (is_shape_pred q)
		| Or (p, q) ->
	    (is_shape_pred p) || (is_shape_pred q)	
		| Forall (_, p) -> is_shape_pred p 
		| Link _ -> true
		| Reach _ -> true	
		| Bool _ -> false
		| _ -> false	
		
(** Instantiation system: instanitate quantified variables using program variables 
		(stored in properties ) *)	
let rec inst_datastruture_proof vc properties = vc 
	(*let preds = split vc in
	let properties = List.fold_left (fun res pred -> 
		match pred with
			| Reach (d, var) -> 
				if (List.mem var properties) then res
				else var::res
			| _ -> res
		) properties preds in
	let preds = List.map (fun pred -> 
		sub_inst properties pred) preds in
	big_and preds
and sub_inst properties pred =			
	match pred with
		| Forall (ps, pr) -> 
			if (is_link pr) then pred
			else 
				if (List.length ps = 1) then
					let q = List.hd ps in
					big_and (pred::(List.map (fun property -> 
						subst (property) q pr
						) properties))
				else if (List.length ps = 2) then
					big_and (pred::(List.map (fun property ->
						let uvar = List.nth ps 0 in
						let vvar = List.nth ps 1 in
						And (
							Forall ([vvar], subst (property) uvar pr),
							Forall ([uvar], subst (property) vvar pr))
						) properties))
					else assert false
		| Or (p1, p2) -> 
			Or (inst_datastruture_proof p1 properties,
					inst_datastruture_proof p2 properties)
		| pred -> pred	*)

(** Collect variables (properties) that should be instantiated in a predicate *)
let find_instantiable_variables preds = []
	(*let rec loop pred = 
		match pred with
			| Iff (px, q) -> loop q
			| Not p -> loop p
			| And (p, q) ->
		    (loop p) @ (loop q)
			| Or (p, q) ->
		    (loop p) @ (loop q)	
			(** only variables in a reach predicate is considered instantiable *)
			(* Fixme: may we need any other way to find variables that should be instantiated *)
			| Reach (d, x) -> [x]
			| Link _ -> []	
			| Forall (_, p) -> []
			| _ -> []	in
	let res = List.fold_left (fun res pred -> 
		res @ (loop pred)
		) [] preds in
	Common.remove_duplicates res*)
	
(* Crazy heuristc -- Fix me. *)	
let approximate pred = 		
	if (is_link pred) then
		let pred = match pred with
			| Forall (foralls, Or (p, q)) ->
				Forall (foralls, Or (p, map_pred (fun pred -> match pred with
					| Not (Atom (Var _, Eq, Var _)) -> True
					| pred -> pred
					) q))
			| pred -> pred in
		match pred with
		| Forall (foralls, Or ((Not (Link _)) as l, composites)) ->
			let composites = split_or composites in
			let linkcomposites, reachcomposites = 
				List.partition (fun composite -> 
					match composite with | Link _ -> true | _ -> false
					) composites in
			(* Working with tree sturctures *)
			if (List.length linkcomposites >= 3) then 
				(* May not get enough samples *)
				if (List.length reachcomposites = 1) then
					let composite = (List.hd reachcomposites) in
					let missingcomposite = match composite with
						| And (Reach (t, u), Atom (v, Eq, x)) 
						| And (Atom (v, Eq, x), Reach (t, u)) -> 
							Some (And (Reach (t, v), Atom (u, Eq, x)))
						| _ -> None in
					match missingcomposite with
						| Some missingcomposite ->
							Forall (foralls, Or (l, big_or (linkcomposites @ [composite; missingcomposite]) ))
						| None -> pred
				else pred
			else (* Translate uncheckable to checkable *)
				let trans_c = ref 0 in
				let composites = List.map (fun composite -> 
					let conjs = split composite in
					let rconjs = List.filter (fun conj -> 
						match conj with | Reach _ -> true | _ -> false
						) conjs in
					let lconjs = List.filter (fun conj ->
						match conj with | Not (Link _) -> true | _ -> false
						) conjs in
					if (List.length rconjs = 2 && lconjs <> [] &&
						List.length conjs = List.length rconjs + List.length lconjs) then
							try
								let rconj1 = List.nth rconjs 0 in
								let rconj2 = List.nth rconjs 1 in
								let r1, r2 = (match rconj1, rconj2 with 
									| Reach (m, u), Reach (n, v) when u <> v -> m, n | _ -> assert false) in
								let _ = assert (r1 = r2) in
								let cs = Hashtbl.create 2 in
													
								(* Do a constructor by constructor analysis *)
								let _ = List.iter (fun lconj -> match lconj with
									| Not (Link (_,c,_,_,_)) ->
										if (Hashtbl.mem cs c) then 
											Hashtbl.replace cs c (lconj::(Hashtbl.find cs c))
										else Hashtbl.replace cs c [lconj]
									| _ -> assert false
									) lconjs in
								let all = Hashtbl.fold (fun c lconjs res -> 
									let u, v = match List.hd lconjs with Not (Link (_,_,_,u,v)) -> u, v | _ -> assert false in
									let lconjs = Common.map_partial (fun lconj -> match lconj with
										| Not (Link (l, c', index, u, v)) when index < 10 -> 
											let _ = assert (l = r1 && c' = c) in
											Some (c', index, u, v) 
										| Not (Link (l, c', _, _, _)) -> (assert (l = r1 && c' = c); None)
										| _ -> None
										) lconjs in
									(* ######## The translation only supports trees now. Fixme. ##########*)
									let _ = assert (List.length lconjs = 2) in
									let ind1 = List.fold_left (fun res (_, i, _, _) -> 
										if i < res then i else res
										) (10) lconjs in
									let _ = assert (ind1 < 10) in
									let ind2 = List.fold_left (fun res (_, i, _, _) -> 
										if i > res then i else res
										) (-1) lconjs in
									let _ = assert (ind2 > (-1) && ind1 < ind2) in
									let ind3 = ind2 * 10 + ind1 in
									let all = [(c, ind1, u, v); (c, ind1, v, u); (c, ind2, u, v); (c, ind2, v, u); (c, ind3, u, v); (c, ind3, v, u)] in
									let all = List.filter (fun a -> List.for_all (fun l -> l <> a) lconjs) all in
									let _ = trans_c := !trans_c + 1 in
									res @ all
									) cs [] in
								big_or (
									List.map (fun (c, ind, u, v) -> Link (r1, c, ind, u, v)
										) all
									)
								with _ -> composite
					else composite
					) composites in
				if !trans_c > 1 then Forall (foralls, Or (l, big_or (composites) )) else pred
		| _ -> pred
	else pred