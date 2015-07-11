type literal =
  | Var
  | Neg

type 'a formula =
  | Lit of literal * 'a
  | And of 'a formula * 'a formula
  | Or  of 'a formula * 'a formula
  | Not of 'a formula

let rec to_nnf f =
	match f with
  | Not f -> (
		match f with
			| (And (f1, f2)) -> 
				Or (to_nnf (Not f1), to_nnf (Not f2))
			| (Or (f1, f2))  -> 
				And (to_nnf (Not f1), to_nnf (Not f2))
			| (Lit (k, l)) -> 
				(match k with 
					| Var -> Lit (Neg, l) 
					| Neg -> Lit (Var, l))
			| (Not f) -> to_nnf f
		)
  | And (f1, f2)       -> 
		And (to_nnf f1, to_nnf f2)
  | Or (f1, f2)        -> 
		Or (to_nnf f1, to_nnf f2)
  | Lit (k, l)              -> 
		Lit (k, l)


let rec dist_or f1 f2 =
  match f1, f2 with
    | Or (f1, f2), Or (f3, f4) -> 
			Or (Or (f1, f2), Or (f3, f4))
    | And (f1, f2), f3         -> 
			And (dist_or f1 f3, dist_or f2 f3)
    | f1, And (f2, f3)         -> 
			And (dist_or f1 f2, dist_or f1 f3)
    | Lit (k1, l1), Lit (k2, l2)           -> 
			Or (f1, f2)
    | Or (f1, f2), Lit (k, l)       -> 
			Or (Or (f1, f2), Lit (k, l))
    | Lit (k, l), Or (f1, f2)       -> 
			Or (Lit (k, l), Or (f1, f2))
    (*| Not _, _                 -> assert (0 = 1); assert false
    | _, Not _                 -> assert (0 = 1); assert false*)

let rec to_cnf f =
	match f with
  | Or (f1, f2)  -> 
		dist_or (to_cnf f1) (to_cnf f2)
  | And (f1, f2) -> 
		And (to_cnf f1, to_cnf f2)
  | Lit (k, l)        -> 
		Lit (k, l)
  (*| Not _        -> assert (0 = 1); assert false*)