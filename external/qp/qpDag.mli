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

(* $Id: fociDag.mli,v 1.5 2005/09/03 04:13:24 mcmillan Exp $
 *
 * This file is modified from the BLAST Project.
 * This file is modified from the FOCI Project.
 *)


(**
 * This module is similar to QpAst, except that it
 * uses a DAG representation for predicates and expressions.
 * Each sub-predicate or sub-expression is paired with
 * a unique ID, which makes hashing elements constant time.
 * Unfortunately, this also makes using DAG's more cumbersome
 * than AST's, because one must always explicitly discard
 * the ID when matching and one must call special functions
 * to add the ID's after constructing nodes.
 *)

module Symbol : 
sig 
  type symbol = string (* change later *)
  val toString : symbol -> string 
  val print : Format.formatter -> symbol -> unit 
end

module Constant :
sig
  type constant = 
      Int     of int		

  val toString : constant -> string
end

type expression = expr_int * int

and expr_int =
    Constant of Constant.constant
  | Variable of Symbol.symbol
  | Application of Symbol.symbol * expression list
  | Sum of expression list
  | Coeff of Constant.constant * expression
  | Ite of predicate * expression * expression
      
and predicate = pred_int * int

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

val ewr : expr_int -> expression
val euw : expression -> expr_int
val pwr : pred_int -> predicate
val puw : predicate -> pred_int

val eConstant : Constant.constant -> expression
val eVariable : Symbol.symbol -> expression
val eApplication : Symbol.symbol * expression list -> expression
val eSum : expression list -> expression
val eCoeff : Constant.constant * expression -> expression
val eIte : predicate * expression * expression -> expression

val pTrue : predicate
val pFalse : predicate
val pAnd : predicate list -> predicate
val pOr : predicate list -> predicate
val pNot : predicate -> predicate
val pImplies : (predicate * predicate) -> predicate
val pEquality : (expression * expression) -> predicate 
val pLeq : (expression * expression) -> predicate 
val pAtom : Symbol.symbol -> predicate
val pForall : (Symbol.symbol list) * predicate -> predicate


module Expression : 
sig

  module Hash : Hashtbl.S with type key = expression

  val print : Format.formatter -> expression -> unit
  val show : expression -> unit
  val toString : expression -> string
    
  val support : expression -> Symbol.symbol list
  val subst : (expression * expression * expression) -> expression
  val map : (expression -> expression) -> expression -> expression
    
end
  
module Predicate :
sig

  module Hash : Hashtbl.S with type key = predicate

  val toString : predicate -> string
  val print : Format.formatter -> predicate -> unit
  val show : predicate -> unit
    
  val support : predicate -> Symbol.symbol list
  val subst : (predicate * expression * expression) -> predicate
  val map : (predicate -> predicate) -> (expression -> expression) -> predicate -> predicate
  val elim_ite : predicate -> predicate
  val size : predicate -> int
end

val zero : expression
val p_one: expression
val m_one: expression

(* a concrete term is either an individual variable
   an application of a function symbol to constants *)

module Concrete :
sig
  type concrete =
      Variable of Symbol.symbol
    | Application of Symbol.symbol * Num.num list
	
  val print : Format.formatter -> concrete -> unit
  val toString : concrete -> string
end

val concretize : (expression * Num.num) list -> (Concrete.concrete * Num.num) list
val pred_tree_of_dag : predicate -> QpAst.predicate
val pred_dag_of_tree : QpAst.predicate -> predicate
val expr_tree_of_dag : expression -> QpAst.expression
val expr_dag_of_tree : QpAst.expression -> expression

(* type theory = RationalLinear | IntegerSeparation *)

val uniq : ('a * int) list -> ('a * int) list

val print_stats : unit -> unit
