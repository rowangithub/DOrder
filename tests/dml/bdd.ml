type bdd = 
	| Zero 
	| One 
	| Node of int * bdd (*low*) * bdd (*high*)

type operator =
  | Op_and | Op_or | Op_imp
  (*| Op_any of (bool -> bool -> bool)*)

let max_var = 10

let rec var b = match b with
  | Zero -> max_var+1 | One -> (*!max_var + 1*) (*11*)max_var+1
  | Node (vv, l, h) -> vv

let low b = match b with
  (*| Zero | One -> invalid_arg "Bdd.low"*)
  | Node (x, l, h) -> l

let high b = match b with
  (*| Zero | One -> invalid_arg "Bdd.low"*)
  | Node (x, l, h) -> h

let var1 b = match b with
	| Zero -> (*20*)max_var+2 | One -> (*20*)max_var+2
	| Node (vv, l, h) -> var l

let var2 b = match b with
	| Zero -> (*20*)max_var+2 | One -> (*20*)max_var+2
	| Node (vv, l, h) -> var h

let mk (vv:int) (low: bdd) (high: bdd) = 
  if low == high then low else Node (vv, low, high)
      
let mk_not x = 
  let rec mk_not_rec x = 
		let res =	match x with
				| Zero -> One
				| One -> Zero
				| Node (vv, l, h) -> mk vv (mk_not_rec l) (mk_not_rec h) 
		in
		res 
	in
  let u = mk_not_rec x in
	let _ = assert (var u >= var x) in u

let of_bool b = if b then One else Zero

let apply_op op b1 b2 = 
	if (op = 0) then (*Op_and ->*) b1 && b2
	else if (op = 1) then (*Op_or  ->*) b1 || b2
	else (*Op_imp ->*) (not b1) || b2
  (*| Op_any f -> f b1 b2*)

let rec app op u1 u2  =
      if (op = 0) then (*| Op_and ->*)
	    if u1 == u2 then 
	      u1
	    else if u1 == Zero || u2 == Zero then
	      Zero
	    else if u1 == One then
	      u2
	    else if u2 == One then
	      u1 
	    else
	      app_gen op u1 u2 
			else if (op = 1) then (*| Op_or ->*)
      if u1 == u2 then
	      u1
	    else if u1 == One || u2 == One then
	      One
	    else if u1 == Zero then
	      u2
	    else if u2 == Zero then
	      u1
	    else 
	      app_gen op u1 u2 
			else (*| Op_imp -> *)
	    if u1 == Zero then
	      One
	    else if u1 == One then
	      u2
	    else if u2 == One then
	      One
	    else
	      app_gen op u1 u2 
 	(*| Op_any _ ->
	    app_gen u1 u2 *)
    and app_gen op u1 u2 = 
      match (u1, u2) with
	| (Zero), (Zero) -> if (apply_op op false false) then One else Zero
	| (Zero), (One) -> if (apply_op op false true) then One else Zero
	| (One), (Zero) -> if (apply_op op true false) then One else Zero 
	| (One), (One) -> if (apply_op op true true) then One else Zero
	| (u1, u2) ->
		let res = 
		let v1 = var u1 in
		let v2 = var u2 in
		if v1 == v2 then
                  mk v1 (app op (low u1) (low u2)) (app op (high u1) (high u2))
                else if v1 < v2 then
		  mk v1 (app op (low u1) u2) (app op (high u1) u2)
		else (* v1 > v2 *)
		  mk v2 (app op u1 (low u2)) (app op u1 (high u2)) 
	      in
	      res 


(*let gapply (op:int) u1 u2 = 
    
    app u1 u2*)

let mk_and u1 u2 = 
	let u' = app 0 u1 u2 in 
	let _ = assert (var u1 <= var u' || var u2 <= var u') in u'(*Op_any (fun b1 b2 -> b1 && b2)*)
let mk_or u1 u2 = 
	let u' = app 1 u1 u2 in
	let _ = assert (var u1 <= var u' || var u2 <= var u') in u' (*Op_any (fun b1 b2 -> b1 || b2)*)
let mk_imp u1 u2 = 
	let u' = app 2 u1 u2 in
	let _ = assert (var u1 <= var u' || var u2 <= var u') in u'
(*Op_any (fun b1 b2 -> (not b1) || b2)*)
(*let mk_iff = gapply (Op_any (fun b1 b2 -> b1 == b2))
let apply f = gapply (Op_any f)*)

type formula = 
  | Ffalse 
  | Ftrue 
  | Fvar of int 
  | Fand of formula *  formula
  | For  of formula *  formula
  | Fimp of formula *  formula
  | Fiff of formula *  formula
  | Fnot of formula

let rec build f = match f with
  | Ffalse -> Zero
  | Ftrue -> One
  | Fvar v -> 
		if (v >= 0 && v < max_var) then Node (v, Zero, One) else Node (4, Zero, One)
  | Fand (f1, f2) -> mk_and (build f1) (build f2)
  | For (f1, f2) -> mk_or (build f1) (build f2)
  | Fimp (f1, f2) -> mk_imp (build f1) (build f2)
  | Fnot f -> mk_not (build f)