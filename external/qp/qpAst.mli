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

(* $Id: fociAst.mli,v 1.6 2004/08/08 23:01:42 mcmillan Exp $
 *
 * This file is modified from the BLAST Project.
 * This file is modified from the FOCI  Project.
 *)


(**
 * This module defines the internal representation used
 * represent expressions and predicates.
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

module Expression : 
sig
  module Hash : Hashtbl.S with type key = expression
  val print : Format.formatter -> expression -> unit
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
  val support : predicate -> Symbol.symbol list
  val subst : (predicate * expression * expression) -> predicate
  val map : (predicate -> predicate) -> (expression -> expression) -> predicate -> predicate
  val elim_ite : predicate -> predicate
end

val zero : expression

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
