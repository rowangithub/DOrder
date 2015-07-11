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

(* qpAst.ml
 *
 * This file is modified from the BLAST Project by KLM.
 * This file is modified from the FOCI Project by Ranjit Jhala.
 *)


(**
 * This module allows the parsing of formulas
 *)

module Misc = QpMisc

module Symbol = 
  struct 
    type symbol = string (* change later *)

    let toString s = "'" ^ s ^ "'"       (* name of symbol *)

    let print fmt s =
      Format.pp_print_string fmt s
  end

module Constant =
  struct
    type constant = 
	Int     of int

    let toString c =
      match c with
	Int i -> string_of_int i
 
  end

type expression =
    Constant of Constant.constant
  | Variable of Symbol.symbol
  | Application of Symbol.symbol * expression list
  | Sum of expression list
  | Coeff of Constant.constant * expression
  | Ite of predicate * expression * expression
      
and predicate =
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

module PredHashStruct = struct
  type t = predicate
  let equal x y = (x == y)
  let hash = Hashtbl.hash
end
  
module PredHash = Hashtbl.Make(PredHashStruct)

let predNum = PredHash.create 251      
let predCount = PredHash.create 251
let predCounter = ref 0

let rec iter_predicate f p =
  let h = PredHash.create 251 in
  let rec z p =
    f p;
    if not(PredHash.mem h p) then
      (PredHash.add h p ();
       match p with
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
  match e with
      Constant ac -> (Constant.toString ac)
    | Application (exp, ev) ->
	(Symbol.toString exp)^
	"("^
	(string_of_expressionList  ev)^")"
    | Variable s -> (Symbol.toString s)
    | Sum(l) -> "+[" ^ (string_of_expressionList l) ^ "]"
    | Coeff(c,t) -> "* " ^ (Constant.toString c) ^ " " ^ (string_of_expression t)
    | Ite(i,t,e) -> "(? " ^ (s_o_p i) ^ " " ^ (string_of_expression t) ^ " " ^ (string_of_expression e) ^ ")"
	
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
  match pred with
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
    
module Expression = 
  struct

    module HashStruct = struct
      type t = expression
      let equal x y = (x == y)
      let hash = Hashtbl.hash
    end
      
    module Hash = Hashtbl.Make(HashStruct)
      
    let rec print fmt e =
      begin
    	match e with
    	Constant ac -> 
	    Format.fprintf fmt "%s" (Constant.toString ac)

	| Application (s, ev) ->
            Format.fprintf fmt "%s" (Symbol.toString s);
	    Format.fprintf fmt "(";
	    List.iter (fun a-> print fmt a) ev;
	    Format.fprintf fmt ")"

    	| Variable s -> 
            Format.fprintf fmt "%s" (Symbol.toString s)

	| Sum(l) -> 
	    Format.fprintf fmt "+[";
	    print_vec fmt l;
	    Format.fprintf fmt "]"

	| Coeff(c,t) ->
	    Format.fprintf fmt "* ";
	    Format.fprintf fmt "%s" (Constant.toString c);
	    Format.fprintf fmt "  ";
	    print fmt t;
        | _ -> Misc.assert_false "match failure" 

      end

    and print_vec fmt ev =
          List.iter (fun a -> print fmt a; Printf.printf " ") ev


    let toString = string_of_expression

    let rec support exp =
      match exp with
      Constant(_) -> []
      | Application (func, args) -> 
	    (List.fold_left Misc.union [func] (List.map support args))
      | Variable(sym) -> [sym]
      | Sum(args) -> (List.fold_left Misc.union [] (List.map support args))
      | Coeff(c,t) -> (support t)
      | _ -> Misc.assert_false "match failure" 


    let rec subst (e,v,vv) =
      let rec s e =
	if e = v then vv else
	  match e with
	      Constant(_) -> e
	    | Application (func, args) -> Application (func, List.map s args)
	    | Variable(_) -> e
	    | Sum(args) -> Sum (List.map s args)
	    | Coeff(c,t) -> Coeff(c,(s t))
            | _ -> Misc.assert_false "match failure" 
      in s e

    let rec map f e =
      let rec s e =
	match e with
	    Constant(_) -> f e
	  | Application (func, args) -> 
	      f (Application (func, List.map s args))
	  | Variable(_) -> f e
	  | Sum(args) -> f (Sum (List.map s args))
	  | Coeff(c,t) -> f (Coeff(c,(s t)))
          | _ -> Misc.assert_false "match failure" 
      in s e
	   

  end

(**
   A predicate is a boolean combination of atomic expressions.
   *)

module Predicate =
    struct


      module HashStruct = struct
	type t = predicate
	let equal x y = (x == y)
	let hash = Hashtbl.hash
      end

      module Hash = Hashtbl.Make(HashStruct)

      let toString = string_of_predicate

      let print fmt pr = Format.pp_print_string fmt (toString pr)


      let rec support pred =
        match pred with
	  True -> []
        | False -> []
        | And plist -> List.fold_left (fun l1 l2 -> Misc.union l1 (support l2)) [] plist
        | Or plist -> List.fold_left (fun l1 l2 -> Misc.union l1 (support l2)) [] plist
        | Not p -> support p
        | Implies (p1, p2) -> Misc.union (support p1) (support p2)
        | Equality (x,y) -> Misc.union (Expression.support x) (Expression.support y)
        | Leq (x,y) -> Misc.union (Expression.support x) (Expression.support y)
	| Atom (s) -> []
        | _ -> Misc.assert_false "match failure" 
      
      let rec subst (e,v,vv) =
	let rec s e =
	  match e with
	      True -> e
            | False -> e
            | And plist -> And(List.map s plist)
            | Or plist -> Or(List.map s plist)
            | Not p -> Not(s p)
            | Implies (p1, p2) -> Implies (s p1, s p2)
            | Equality (x,y) -> Equality(Expression.subst(x,v,vv),Expression.subst (y,v,vv))
	    | Atom (_) -> e
	    | Leq(x,y) -> Leq(Expression.subst(x,v,vv),Expression.subst (y,v,vv))
            | _ -> Misc.assert_false "match failure" 
      in s e

      let rec map f fe e =
	let h = Hash.create 251 in
	let rec s e =
	  try Hash.find h e
	  with Not_found -> (let foo = s1 e in Hash.add h e foo; foo)
	and s1 e =
	  match e with
	      True -> f e
            | False -> f e
            | And plist -> f (And(List.map s plist))
            | Or plist -> f (Or(List.map s plist))
            | Not p -> f (Not(s p))
            | Implies (p1, p2) -> f (Implies (s p1, s p2))
            | Equality (x,y) -> f (Equality(Expression.map fe x, Expression.map fe y))
	    | Atom (_) -> f e
	    | Leq(x,y) -> f (Leq(Expression.map fe x, Expression.map fe y))
            | _ -> Misc.assert_false "match failure" 
          in s e
	     
      (* Translate predicate to a satisfiability-equivalent predicate without Ite *)
	     
      let temp_ctr = ref 0
      let new_temp () =
	let n = "$$$" ^ (string_of_int !temp_ctr) in
	  (temp_ctr := !temp_ctr + 1; n)
	  
      let elim_ite sp =
	let cnsts = ref [] in
	let rec te e =
	  match e with
	      Constant(c) -> Constant(c)
	    | Application (func, args) -> 
		Application (func, List.map te args)
	    | Variable(v) -> Variable(v)
	    | Sum(args) -> Sum(List.map te args)
	    | Coeff(c,t) -> Coeff(c,te t)
	    | Ite(si,st,se) ->
		let temp = Variable(new_temp()) in
		let i = tp si in
		  begin
		    cnsts := Or [Not(i); Equality(temp,(te st))] :: (!cnsts);
		    cnsts := Or [i; Equality(temp,(te se))] :: (!cnsts);
		    temp
		  end
	and tp p = 
	  match p with
	      True -> True
	    | False -> False
	    | And plist -> And (List.map tp plist)
	    | Or plist -> Or (List.map tp plist)
	    | Not p -> Not (tp p)
	    | Implies (p1, p2) -> Implies((tp p1),(tp p2))
	    | Equality (x,y) -> Equality((te x),(te y))
	    | Leq (x,y) -> Leq((te x),(te y))
	    | Atom (s) -> Atom(s)
            | _ -> Misc.assert_false "match failure" in
	let foo = tp sp in
	  And(foo :: !cnsts)
    end

let zero = Constant(Constant.Int(0))

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
	    Format.fprintf fmt "(";
	    List.iter (fun a -> Format.fprintf fmt "%s " (Num.string_of_num a)) ev;
	    Format.fprintf fmt ")"
    end
    
    
  let rec toString e =
    begin
      match e with
    	  Variable s ->  (Symbol.toString s)
	| Application (exp, ev) ->
	    (Symbol.toString exp)^
	    "("^
	    (toStringList  ev)^")"
    end
    
  and toStringList  ev =
    List.fold_left (fun a -> fun b -> a ^ " " ^ (Num.string_of_num b)) "" ev
end
  
