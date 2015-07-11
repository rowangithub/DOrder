(*
 * Copyright © 1990-2002 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(*
 * This file is modified from the BLAST Project by KLM.
 * This file is modified from the FOCI Project by Ranjit Jhala.
 *)

module Symbol = 
  struct 
    type symbol = string (* change later *)

    let is_alpha c = 
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
      
    let is_digit c = 
      (c >= '0' && c <= '9')
      
    let is_safe s =
      let n = String.length s in
      let rec is1 i =
	if i >= n then true
	else let c = s.[i] in
	  if (is_alpha c) || (is_digit c) || c = '_' then
	    is1 (i+1) else false in
	(is_alpha s.[0] && (is1 1))

    let toString s = if (is_safe s) then s else "'" ^ s ^ "'"      (* name of symbol *)

    let print fmt s =
      if is_safe s then
	Format.pp_print_string fmt s
      else (
	Format.pp_print_string fmt "'";
	Format.pp_print_string fmt s;
	Format.pp_print_string fmt "'")

  end

module Constant =
  struct
    type constant = 
	Int     of int

    let toString c =
      match c with
	Int i -> string_of_int i
 
  end

type expression =  expr_int * int

and expr_int =
    Constant of Constant.constant
  | Variable of Symbol.symbol
  | Application of Symbol.symbol * expression list
  | Sum of expression list
  | Coeff of Constant.constant * expression
  | Ite of predicate * expression * expression
      
and predicate =  pred_int * int

and pred_int =
    True
  | False
  | And of predicate list
  | Or of predicate list
  | Not of predicate
  | Implies of predicate * predicate
  | Equality of expression * expression
  | Leq of expression * expression
  | Atom of Symbol.symbol
  | Forall of (Symbol.symbol list) * predicate


let seq x y = (x == y) 

module Hashcons (X : sig type t val sub_equal : t -> t -> bool val hash : t -> int end) = struct
  module HashStruct = struct
    type t = X.t * int
    let equal (x,_) (y,_) = X.sub_equal x y
    let hash (x,_) = X.hash x
  end
  module Hash = Weak.Make(HashStruct)
  let utab = Hash.create 251
  let ctr = ref 0
  let wrap e = 
    let foo = (e,!ctr) in
    let res = Hash.merge utab foo in
      if (match res with (_,id) -> id) = !ctr then ctr := !ctr + 1;
      res
  let unwrap (e,_) = e
end

module ExprHashconsStruct = struct
  type t = expr_int
  let sub_equal x y =
    match x,y with
      | Constant x, Constant y -> x = y
      | Variable x, Variable y -> x = y
      | Application (s1, l1), Application (s2,l2) ->
	  (s1 = s2) && (try List.for_all2 seq l1 l2 with _ -> false)
      | Sum l1, Sum l2 -> (try List.for_all2 seq l1 l2 with _ -> false)
      | Coeff (c,x), Coeff (c',x') -> c = c' && (seq x x')
      | Ite (i,t,e), Ite (i',t',e') ->
	  (seq i i') && (seq t t') && (seq e e')
      | _,_ -> false
  let hash e =
    let rec list_hash v =
      function 
	  [] -> v
	| (_,id) :: tl -> list_hash ((v * 2) + id) tl in
      match e with
	| Constant (Constant.Int x) -> x
	| Variable x -> Hashtbl.hash x
	| Application (s, l) -> list_hash ((Hashtbl.hash s) + 1) l
	| Sum l -> list_hash 2 l
	| Coeff ((Constant.Int c),(_,id)) -> 12 + (2 * c) + id
	| Ite ((_,id1),(_,id2),(_,id3)) -> 32 + (4 * id1) + (2 * id2) + id3
end
  
module ExprHashcons = Hashcons(ExprHashconsStruct)

module PredHashconsStruct = struct
  type t = pred_int
  let sub_equal x y =
    match x,y with
      | True, True -> true
      | False, False -> true
      | And(l), And(l') -> (try List.for_all2 seq l l' with _ -> false)
      | Or(l), Or(l') -> (try List.for_all2 seq l l' with _ -> false)
      | Not x, Not y -> seq x y
      | Implies(x,y), Implies(x',y') -> (seq x x') && (seq y y')
      | Equality(x,y), Equality(x',y') -> (seq x x') && (seq y y')
      | Leq(x,y), Leq(x',y') -> (seq x x') && (seq y y')
      | Atom x, Atom y -> x = y
      | Forall(x,y), Forall(x',y') -> (x = x') && (seq y y')
      | _,_ -> false
  let hash p =
    let rec list_hash v =
      function 
	  [] -> v
	| (_,id) :: tl -> list_hash ((v * 2) + id) tl in
      match p with
	| True -> 0
	| False -> 1
	| And(l) -> list_hash 2 l
	| Or(l) -> list_hash 3 l
	| Not (_,id) -> 8 + id 
	| Implies((_,id1),(_,id2)) -> 20 + (2 * id1) + id2
	| Equality((_,id1),(_,id2)) -> 24 + (2 * id1) + id2
	| Leq((_,id1),(_,id2)) -> 28 + (2 * id1) + id2
	| Atom x -> Hashtbl.hash x
	| Forall(vs,(_,id)) -> 32 + (2 * (Hashtbl.hash vs)) +  id
end
  
module PredHashcons = Hashcons(PredHashconsStruct)
  
let ewr = ExprHashcons.wrap
let euw = ExprHashcons.unwrap
let pwr p = let foo = PredHashcons.wrap p in foo
let puw = PredHashcons.unwrap
	    
let eConstant c = ewr(Constant c)
let eVariable s = ewr(Variable s)
let eApplication (s,l) = ewr(Application(s,l))
let eSum l = ewr(Sum l)
let eCoeff (c,e) = ewr(Coeff(c,e))
let eIte (i,t,e) = ewr(Ite(i,t,e))

let pTrue = pwr True
let pFalse = pwr False
let pAnd l = pwr(And l)
let pOr l = pwr (Or l)
let pNot p = pwr(Not p)
let pImplies (p,q) = pwr(Implies(p,q))
let pEquality (e,f) = pwr(Equality(e,f))
let pLeq (e,f) = pwr(Leq(e,f))
let pAtom s = pwr(Atom(s))
let pForall (vs,bd) = pwr(Forall(vs,bd))
			
module PredHashStruct = struct
  type t = predicate
  let equal (_,x) (_,y) = (x = y)
  let hash (_,x) = x
end

module PredHash = Hashtbl.Make(PredHashStruct)

module ExprHashStruct = struct
  type t = expression
  let equal (_,x) (_,y) = (x = y)
  let hash (_,x) = x
end
  
module ExprHash = Hashtbl.Make(ExprHashStruct)

let predNum = PredHash.create 251      
let predCount = PredHash.create 251
let predCounter = ref 0

let rec iter_predicate f p =
  let h = PredHash.create 251 in
  let rec z p =
    f p;
    if not(PredHash.mem h p) then
      (PredHash.add h p ();
       match puw p with
	   True -> ()
	 | False -> ()
	 | And plist -> (List.iter z plist)
	 | Or plist -> (List.iter z plist)
	 | Not q -> (z q)
	 | Implies (p1, p2) -> (z p1; z p2)
	 | Equality (x,y) -> ()
	 | Leq (x,y) -> ()
	 | Atom (s) -> ()
	 | Forall (l,bd) -> (z bd))
  in
    z p
      
let incr_pred_hash h p = 
  PredHash.replace h p
    (try (PredHash.find h p) + 1 with Not_found -> 1)
    
let count_predicate p =
  PredHash.clear predCount;
  iter_predicate (incr_pred_hash predCount) p
		    
let rec string_of_expression e =
  match euw e with
      Constant ac -> (Constant.toString ac)
    | Application (exp, ev) ->
	(Symbol.toString exp)^
	"["^
	(string_of_expressionList  ev)^"]"
    | Variable s -> (Symbol.toString s)
    | Sum(l) -> "+[" ^ (string_of_expressionList l) ^ "]"
    | Coeff(c,t) -> "(* " ^ (Constant.toString c) ^ " " ^ (string_of_expression t) ^ ")"
    | Ite(i,t,e) -> "? " ^ (s_o_p i) ^ " " ^ (string_of_expression t) ^ " " ^ (string_of_expression e)
	
and string_of_expressionList  ev =
  List.fold_left (fun a -> fun b -> a ^ " " ^ (string_of_expression b)) "" ev
    
and s_o_p pred =
  try "@" ^ (string_of_int (PredHash.find predNum pred))
  with Not_found ->
    if not (PredHash.mem predCount pred) then
      (s_o_p1 pred)
    else if (PredHash.find predCount pred) > 1 then
      (predCounter := !predCounter + 1;
       let tag = !predCounter in
	 PredHash.add predNum pred !predCounter;
	 "#" ^ (string_of_int (tag)) ^ " " ^ (s_o_p1 pred))
    else (s_o_p1 pred)
      
and s_o_p1 pred =
  match puw pred with
      True -> "true"
    | False -> "false"
    | And plist -> "& ["^(s_o_pList plist)^"]"
    | Or plist -> "| ["^(s_o_pList plist)^"]"
    | Not p -> "~ ("^(s_o_p p)^")"
    | Implies (p1, p2) -> "("^(s_o_p p1)^"->"^(s_o_p p2)^")"
    | Equality (x,y) -> "(= "^(string_of_expression x)^" "^(string_of_expression y)^")"
    | Leq (x,y) -> "(<= "^(string_of_expression x)^" "^(string_of_expression y)^")"
    | Atom (s) -> (Symbol.toString s)
    | Forall (l,p) -> ":[" 	^ (List.fold_left (fun a -> fun b -> a ^ " " ^ (Symbol.toString b)) "" l) ^ "]" ^ (s_o_p p)
	
and s_o_pList plist =
	List.fold_left (fun a -> fun b -> a ^ " " ^ (s_o_p b)) "" plist

let string_of_predicate p =
  PredHash.clear predNum;
  count_predicate p;
  s_o_p p
    
let rec pred_map h he f fe e =
  let rec s e =
    try PredHash.find h e
    with Not_found -> (let foo = s1 e in PredHash.add h e foo; foo)
  and s1 e =
    match puw e with
	True -> f e
      | False -> f e
      | And plist -> f (pwr (And(List.map s plist)))
      | Or plist -> f (pwr (Or(List.map s plist)))
      | Not p -> f (pwr (Not(s p)))
      | Implies (p1, p2) -> f (pwr (Implies (s p1, s p2)))
      | Equality (x,y) -> f (pwr (Equality(expr_map h he f fe x, expr_map h he f fe y)))
      | Atom (_) -> f e
      | Leq(x,y) -> f (pwr (Leq(expr_map h he f fe x, expr_map h he f fe y)))
  in s e

and expr_map h he fp f e =
  let rec s e =
    try ExprHash.find he e with
	Not_found -> (let foo = s1 e in ExprHash.add he e foo; foo)
  and s1 e =
    match euw e with
	Constant(_) -> f e
      | Application (func, args) -> 
	  f (ewr (Application (func, List.map s args)))
      | Variable(_) -> f e
      | Sum(args) -> f (ewr (Sum (List.map s args)))
      | Coeff(c,t) -> f (ewr (Coeff(c,(s t))))
      | Ite(i,t,e) -> f (ewr (Ite(pred_map h he fp f i, s t, s e)))
  in s e


let rec expr_subst hp h e v vv =
  let rec s e =
    try ExprHash.find h e with
	Not_found -> (let foo = s1 e in ExprHash.add h e foo; foo)
  and s1 e =
    if e == v then vv
    else
      match euw e with
	  Constant(_) -> e
	| Application (func, args) -> 
	    ewr (Application (func, List.map s args))
	| Variable(_) -> e
	| Sum(args) -> ewr (Sum (List.map s args))
	| Coeff(c,t) -> ewr (Coeff(c,(s t)))
	| Ite(a,b,c) -> eIte (pred_subst hp h a v vv, s b, s c)
  in s e

and pred_subst h he e v vv =
  let rec s e =
    try PredHash.find h e with
	Not_found -> (let foo = s1 e in PredHash.add h e foo; foo)
  and s1 e =
    match puw e with
	True -> e
      | False -> e
      | And plist -> pwr (And(List.map s plist))
      | Or plist -> pwr (Or(List.map s plist))
      | Not p -> pwr (Not(s p))
      | Implies (p1, p2) -> pwr (Implies (s p1, s p2))
      | Equality (x,y) -> pwr (Equality(expr_subst h he x v vv,expr_subst h he y v vv))
      | Atom (_) -> e
      | Leq(x,y) -> pwr (Leq(expr_subst h he x v vv, expr_subst h he y v vv))
  in s e
       

module Expression = 
  struct

    module HashStruct = struct
      type t = expression
      let equal (_,x) (_,y) = (x = y)
      let hash (_,x) = x
    end
      
    module Hash = Hashtbl.Make(HashStruct)
      
    let toString = string_of_expression

    let rec print fmt e = Format.pp_print_string fmt (toString e)

    let show = print Format.std_formatter

    let support exp =
      let h = Hash.create 251 in
      let sh = Hashtbl.create 251 in
      let res = ref [] in
      let add s = if not(Hashtbl.mem sh s) then Hashtbl.add sh s (); res := s :: !res in
      let rec s exp =
	try Hash.find h exp with
	    Not_found -> Hash.add h exp (); s1 exp
      and s1 exp =
	match euw exp with
	    Constant(_) -> ()
	  | Application (func, args) -> 
	      add func; List.iter s args
	  | Variable(sym) -> add sym
	  | Sum(args) -> List.iter s args
	  | Coeff(c,t) -> s t
      in s exp; List.rev !res
	   
    let subst (e,v,x) =
      let hp = PredHash.create 251 in
      let he = ExprHash.create 251 in
	expr_subst hp he e v x
	   
    let rec map f e =
      let h = Hash.create 251 in
      let rec s e =
	try Hash.find h e with
	    Not_found -> (let foo = s1 e in Hash.add h e foo; foo)
      and s1 e =
	match euw e with
	    Constant(_) -> f e
	  | Application (func, args) -> 
	      f (ewr (Application (func, List.map s args)))
	  | Variable(_) -> f e
	  | Sum(args) -> f (ewr (Sum (List.map s args)))
	  | Coeff(c,t) -> f (ewr (Coeff(c,(s t))))
      in s e
	   
	   
  end
    
(**
   A predicate is a boolean combination of atomic expressions.
   *)

module Predicate =
    struct
      module HashStruct = struct
	type t = predicate
	let equal (_,x) (_,y) = (x = y)
	let hash (_,x) = x
      end
	
      module Hash = Hashtbl.Make(HashStruct)
	
      let toString = string_of_predicate
		       
      let print fmt pr = Format.pp_print_string fmt (toString pr)
			   
      let show = print Format.std_formatter
			   
      let rec support pred =
	let h = Hash.create 251 in
	let eh = Expression.Hash.create 251 in
	let sh = Hashtbl.create 251 in
	let res = ref [] in
	let add s = if not(Hashtbl.mem sh s) then Hashtbl.add sh s (); res := s :: !res in

	let se exp =
	  let rec s exp =
	    try Expression.Hash.find eh exp with
		Not_found -> Expression.Hash.add eh exp (); s1 exp
	  and s1 exp =
	    match euw exp with
		Constant(_) -> ()
	      | Application (func, args) -> 
		  add func; List.iter s args
	      | Variable(sym) -> add sym
	      | Sum(args) -> List.iter s args
	      | Coeff(c,t) -> s t
	      | Ite _ -> failwith "ite not supported"
	  in s exp in
	  
	let rec s exp =
	  try Hash.find h exp with
	      Not_found -> Hash.add h exp (); s1 exp
	and s1 pred =
          match puw pred with
	      True -> ()
            | False -> ()
            | And plist -> List.iter s plist
            | Or plist -> List.iter s plist
            | Not p -> s p
            | Implies (p1, p2) -> s p1; s p2
            | Equality (x,y) -> se x; se y
            | Leq (x,y) -> se x; se y
	    | Atom (s) -> ()
	in s pred; List.rev !res
	    
	     
(*      let rec map f fe e =
	let h = Hash.create 251 in
	let rec s e =
	  try Hash.find h e
	  with Not_found -> (let foo = s1 e in Hash.add h e foo; foo)
	and s1 e =
	  match puw e with
	      True -> f e
            | False -> f e
            | And plist -> f (pwr (And(List.map s plist)))
            | Or plist -> f (pwr (Or(List.map s plist)))
            | Not p -> f (pwr (Not(s p)))
            | Implies (p1, p2) -> f (pwr (Implies (s p1, s p2)))
            | Equality (x,y) -> f (pwr (Equality(Expression.map fe x, Expression.map fe y)))
	    | Atom (_) -> f e
	    | Leq(x,y) -> f (pwr (Leq(Expression.map fe x, Expression.map fe y)))
	in s e *)

      let map f fe e =
	let h = PredHash.create 251 in
	let he = ExprHash.create 251 in
	  pred_map h he f fe e
	    
      let subst (e,v,x) =
	let hp = PredHash.create 251 in
	let he = ExprHash.create 251 in
	  pred_subst hp he e v x
	    
      let size p =
	let cnt = ref 0 in
	  (iter_predicate (fun _ -> cnt := !cnt + 1)  p; !cnt)

      (* Translate predicate to a satisfiability-equivalent predicate without Ite *)
	     
      let temp_ctr = ref 0
      let new_temp () =
	let n = "$$$" ^ (string_of_int !temp_ctr) in
	  (temp_ctr := !temp_ctr + 1; n)
	  
      let elim_ite sp =
	let cnsts = ref [] in
	let he = Expression.Hash.create 251 in
	let hp = Hash.create 251 in
	let rec te e =
	  try Expression.Hash.find he e
	  with Not_found -> (let foo = te1 e in Expression.Hash.add he e foo; foo)
	and te1 e =
	  match euw e with
	      Constant(c) -> e
	    | Application (func, args) -> 
		ewr (Application (func, List.map te args))
	    | Variable(v) -> ewr (Variable(v))
	    | Sum(args) -> ewr (Sum(List.map te args))
	    | Coeff(c,t) -> ewr (Coeff(c,te t))
	    | Ite(si,st,se) ->
		let temp = ewr (Variable(new_temp())) in
		let i = tp si in
		let tv = te st and ev = te se in
		  begin
		    cnsts := pwr (Or [pwr (Not i); pwr (Equality(temp,(tv)))]) :: (!cnsts);
		    cnsts := pwr (Or [i; pwr (Equality(temp,(ev)))]) :: (!cnsts);
		    temp
		  end
	and tp p = 
	  try Hash.find hp p
	  with Not_found -> (let foo = tp1 p in Hash.add hp p foo; foo)
	and tp1 p =
	  match puw p with
	      True -> p
	    | False -> p
	    | And plist -> pwr (And (List.map tp plist))
	    | Or plist -> pwr (Or (List.map tp plist))
	    | Not p -> pwr (Not (tp p))
	    | Implies (p1, p2) -> pwr (Implies((tp p1),(tp p2)))
	    | Equality (x,y) -> pwr(Equality((te x),(te y)))
	    | Leq (x,y) -> pwr(Leq((te x),(te y)))
	    | Atom (s) -> p
	in
	let foo = tp sp in
	  pwr (And(foo :: !cnsts))
    end
      
let zero  = ewr (Constant(Constant.Int(0)))
let p_one = ewr (Constant(Constant.Int(1)))
let m_one = ewr (Constant(Constant.Int(-1)))

module Concrete = 
struct
  type concrete =
      Variable of Symbol.symbol
    | Application of Symbol.symbol * Num.num list
	
  let rec print fmt e =
    begin
      match e with
    	  Variable s -> 
	    Format.fprintf fmt "%s" (Symbol.toString s)
	| Application (s, ev) ->
            Format.fprintf fmt "%s" (Symbol.toString s);
	    Format.fprintf fmt "[";
	    List.iter (fun a -> Format.fprintf fmt "%s " (Num.string_of_num a)) ev;
	    Format.fprintf fmt "]"
    end
    
    
  let rec toString e =
    begin
      match e with
    	  Variable s ->  (Symbol.toString s)
	| Application (exp, ev) ->
	    (Symbol.toString exp)^
	    "["^
	    (toStringList  ev)^"]"
    end
    
  and toStringList  ev =
    List.fold_left (fun a -> fun b -> a ^ " " ^ (Num.string_of_num b)) "" ev

  module HashStruct = struct
    type t = concrete
    let equal x y = (x = y)
    let hash = Hashtbl.hash
  end
    
  module Hash = Hashtbl.Make(HashStruct)
    
    
      
end
  

(* Make a theory model fully concrete *)
  
let concretize a = 
  let h = ExprHash.create 251 in
  let k = Concrete.Hash.create 251 in
  let rec eval x =
    match x with
	Constant(Constant.Int c),_ -> (Num.num_of_int c)
      | Sum l,_ ->
	  List.fold_left Num.add_num (Num.num_of_int 0) (List.map eval l)
      | Coeff (Constant.Int(c),e),_ -> Num.mult_num (Num.num_of_int c) (eval e)
      | x -> 
	  try ExprHash.find h x 
	  with Not_found -> (failwith "no value for term")
  in
  let res = ref [] in
    begin
      List.iter (function (e,v) ->
		   ExprHash.add h e v) a;
      List.iter
	(function (e,v) ->
	   let lhs = match e with
	       Application(s,l),_ ->
		 Concrete.Application(s,(List.map eval l))
	     | Variable(s),_ -> Concrete.Variable(s) in
	     try Concrete.Hash.find k lhs with
		 Not_found ->
		   begin
		     res := (lhs,v) :: !res;
		     Concrete.Hash.add k lhs ()
		   end) a;
      !res
    end
    

let tree_of_dag () =
  let hp = PredHash.create 251 in
  let he = ExprHash.create 251 in
  let rec ptod e =
    try PredHash.find hp e
    with Not_found -> (let foo = ptod1 e in PredHash.add hp e foo; foo)
  and ptod1 e =
    match puw e with
	True -> QpAst.True
      | False -> QpAst.False
      | And plist -> QpAst.And(List.map ptod plist)
      | Or plist -> QpAst.Or(List.map ptod plist)
      | Not p -> QpAst.Not(ptod p)
      | Implies (p1, p2) -> QpAst.Implies (ptod p1, ptod p2)
      | Equality (x,y) -> QpAst.Equality(etod x, etod y)
      | Atom (s) -> QpAst.Atom(s)
      | Leq(x,y) -> QpAst.Leq(etod x, etod y)
  and etod e =
    try ExprHash.find he e with
	Not_found -> (let foo = etod1 e in ExprHash.add he e foo; foo)
  and etod1 e =
    match euw e with
	Constant(Constant.Int(v)) -> QpAst.Constant(QpAst.Constant.Int(v))
      | Application (func, args) -> 
	  QpAst.Application (func, List.map etod args)
      | Variable(s) -> QpAst.Variable(s)
      | Sum(args) -> QpAst.Sum (List.map etod args)
      | Coeff(Constant.Int(c),t) -> QpAst.Coeff (QpAst.Constant.Int(c), etod t)
      | Ite(i,t,e) -> QpAst.Ite(ptod i, etod t, etod e)
  in (ptod,etod)
       
let pred_tree_of_dag p =
  let (ptod,_) = tree_of_dag () in
    ptod p

let expr_tree_of_dag e =
  let (_,etod) = tree_of_dag () in
    etod e
      
let dag_of_tree () =
  let rec te e =
    match e with
	QpAst.Constant(QpAst.Constant.Int(c)) -> ewr(Constant(Constant.Int(c)))
      | QpAst.Application (func, args) -> 
	  ewr (Application (func, List.map te args))
      | QpAst.Variable(v) -> ewr (Variable(v))
      | QpAst.Sum(args) -> ewr (Sum(List.map te args))
      | QpAst.Coeff(QpAst.Constant.Int(c),t) -> ewr (Coeff(Constant.Int(c),te t))
      | QpAst.Ite(si,st,se) -> ewr(Ite(tp si, te st, te se))
  and tp p = 
    match p with
	QpAst.True -> pwr True
      | QpAst.False -> pwr False
      | QpAst.And plist -> pwr (And (List.map tp plist))
      | QpAst.Or plist -> pwr (Or (List.map tp plist))
      | QpAst.Not p -> pwr (Not (tp p))
      | QpAst.Implies (p1, p2) -> pwr (Implies((tp p1),(tp p2)))
      | QpAst.Equality (x,y) -> pwr(Equality((te x),(te y)))
      | QpAst.Leq (x,y) -> pwr(Leq((te x),(te y)))
      | QpAst.Atom (s) -> pwr(Atom(s))
  in
    (tp,te)

let pred_dag_of_tree p =
  let (pdot,_) = dag_of_tree () in
    pdot p

let expr_dag_of_tree e =
  let (_,edot) = dag_of_tree () in
    edot e

type theory = RationalLinear | IntegerSeparation

let uniq l =
  let rec foo = function
      [] -> []
    | hd :: tl -> match (foo tl) with
	  [] -> [hd]
	| (hd' :: tl') as bar -> if (hd' == hd) then bar else hd :: bar in
    foo (List.sort (function (_,i) -> function (_,j) -> (i-j)) l)

exception Foo

let print_stats _ =
  Printf.printf "preds: %d, exprs: %d" (PredHashcons.Hash.count PredHashcons.utab) (ExprHashcons.Hash.count ExprHashcons.utab);
  print_newline ();
  let cnt = ref 0 in
    (try
       PredHashcons.Hash.iter (fun p -> Predicate.print Format.std_formatter p;
				 cnt := !cnt + 1;
				 if !cnt = 100 then raise Foo)  PredHashcons.utab
     with Foo -> ());
    Format.print_newline ()
      
