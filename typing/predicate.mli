open Format

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
  | Field of string * pexpr     (* INVARIANT: disjoint fields in same module *)
  | Proj of int * pexpr

type t =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of pexpr * t
  | Not of t
  | And of t * t 
  | Or of t * t
	(* Reachability predicates: Reach (u, v) means v is reachable from u *)
	| Reach of pexpr * pexpr
	(* Link predicates: Link (d, constructor (c), field (f), u, v) means u -->c.f v;
	 u and v are reachable from d *)
	| Link of pexpr * string * int * pexpr * pexpr
	(* Universally quantified predicates *)
	| Forall of (Path.t list) * t
	| Bool of pexpr
	
val pprint_op : binop -> string
val pprint_rel: binrel -> string
val pprint: formatter -> t -> unit
val pprint': formatter -> t -> unit
val pprint_pexpr: formatter -> pexpr -> unit
val pprint_pexpr': formatter -> pexpr -> unit

val big_and: t list -> t
val big_or: t list -> t
val equals: (pexpr * pexpr) -> t
val implies: (t * t) -> t
val int_true: pexpr
val int_false: pexpr
val expand_iff: t -> t

val (==.): pexpr -> pexpr -> t
val (!=.): pexpr -> pexpr -> t
val (<=.): pexpr -> pexpr -> t
val (<.): pexpr -> pexpr -> t
val (>=.): pexpr -> pexpr -> t
val (>.): pexpr -> pexpr -> t
val (&&.): t -> t -> t
val (||.): t -> t -> t
val (!.): t -> t
val (=>.): t -> t -> t
val (<=>.): pexpr -> t -> t
val (+-): pexpr -> pexpr -> pexpr
val ( *-): pexpr -> pexpr -> pexpr
val ( /-): pexpr -> pexpr -> pexpr
val (--): pexpr -> pexpr -> pexpr
val logic_equals: t -> t -> t

val subst: pexpr -> Path.t -> t -> t
val apply_substs: (Path.t * pexpr) list -> t -> t
val exp_apply_substs : (Path.t * pexpr) list -> pexpr -> pexpr
val exp_vars : pexpr -> Path.t list
val vars: t -> Path.t list
val ints: t -> int list
val coeffs : t -> int list
val quantifiers : t -> Path.t list
(* pmr: change to plain old instantiate *)
val instantiate_named_vars: (string * Path.t) list -> t -> t
val transl_op: Asttypes.predexp_op -> binop                                                             
val transl_rel: Asttypes.pred_rel -> binrel

val map_quantifiers : (Path.t -> Path.t) -> t -> t
val map_expr_from_top : (pexpr -> pexpr) -> t -> t
val map_expr : (pexpr -> pexpr) -> t -> t
val map_pred : (t -> t) -> t -> t
val map_pred_from_bottom : (t -> t) -> t -> t
val get_all_funs : t -> pexpr list
val exp_var : pexpr -> Path.t
val split : t -> t list
val fullsplit : t -> t list

val inst_forall :  t -> pexpr list -> t
val deduct_datastructure_proof : t -> t	
val inst_datastruture_proof: t -> pexpr list -> t	
val find_instantiable_variables : t list -> pexpr list		
val is_shape_pred : t -> bool	
val approximate_inequlities : t -> t		