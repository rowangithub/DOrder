(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
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

(* This file is part of the SIMPLE Project.*)

open Predicate

module type PROVER = 
  sig
    (* push p: tell prover to assume fact p *)
    val push : Predicate.t -> unit 
    
    (* pop () : tell prover to un-assume last assumed fact *)
    val pop : unit -> unit 
    
    (* reset (): tell prover to remove all assumed facts *)
    val reset : unit -> unit 
    
    (* valid p : do the currently assumed facts imply p ? *)
    val valid : Predicate.t -> bool

    (* implies (p, q) = true iff predicate p (provably) implies predicate q *)
    val implies : (Predicate.t * Predicate.t) -> bool

    val print_simplify: Predicate.t -> Predicate.t -> unit

    val restart_simplify : unit -> unit
  end

module M = Message

module SimplifyProver : PROVER = 
  struct 
(***************************************************************************)
(**** Interface to Simplify, run as a server and queries are piped in ******)
(***************************************************************************)

let fixed_simplify_axioms = ref 	    
[(* "(BG_PUSH (FORALL (x y) (EQ (select (addrOf x y) 0) x)))" ;
  "(BG_PUSH (FORALL (x y d1) (IMPLIES (EQ (foffset x d1) (foffset y d1)) (EQ  x y))))\n" ;
  "(BG_PUSH (FORALL (x y) (NEQ (addrOf x y) 0)))" ;
  "(BG_PUSH (FORALL (x y) (EQ (* (Div x y) y) x )) ) " ;
  "(BG_PUSH (FORALL (x) (NEQ (_STRINGCONSTANT x) 0 ))) " ;
  "(BG_PUSH (FORALL (s t x ) (IMPLIES (EQ s (add t x)) (EQ (in s x) 1))))";
  "(BG_PUSH (FORALL (s x) (IMPLIES (EQ s (emptyset )) (NEQ (in s x) 1))))" *)]


(***************************************************************************)
(************ utilities for piping queries *********************************)
(***************************************************************************)
exception ChannelException

let simplifyServer = ref None
let simplify_stack_counter = ref 0
let current_simplify_stack = ref [] 
let simplifyServerCount = ref 0 
let simplifyCacheCount = ref 0 
let simplify_query_flag = ref false 

let reset_alarm () = ()

let set_alarm () = ()

let simp_oc = if !Clflags.log_queries then open_out "simplify.log" else open_out "/dev/null"

(* may raise ChannelException *)
let secure_output_string oc s = 
  try set_alarm (); 
  output_string simp_oc s; flush simp_oc;
  output_string oc s; flush oc; reset_alarm ()
  with ChannelException -> reset_alarm (); raise ChannelException

(* may raise ChannelException *)
let secure_input_line ic =  
  set_alarm ();
  try
    let rv = input_line ic in 
    reset_alarm (); rv
  with ChannelException ->
    reset_alarm () ;
    raise ChannelException

let getServer () =
  match !simplifyServer with Some (a,b,_) -> (a,b) 
  | None -> 
      M.msg_string M.Debug "Forking Simplify process..." ;
      let ic,oc,ec = Unix.open_process_full "Simplify" (Unix.environment ()) in
      simplifyServer := Some(ic,oc,ec) ;
      List.iter (fun x -> secure_output_string oc (x^"\n")) !fixed_simplify_axioms ;
      M.msg_string M.Debug "done!\n";
      flush stdout ;
      at_exit (fun () -> ignore (Unix.close_process_full (ic,oc,ec)););
      (ic,oc)
and kill_simplify_server () = 
  match !simplifyServer with None -> ()
    | Some (ic,oc,ec) -> 
        try
	  M.msg_string M.Normal "Killing simplify server...";
	  ignore(Sys.command "killall Simplify");
	  simplifyServer := None;
	  M.msg_string M.Normal "Done killing ..." ;
	with _ -> failwith " failed to kill simplify !"

let restart_simplify () = 
  M.msg_string M.Normal "restarting simplify ...";
  kill_simplify_server ();
  let (ic,oc) = getServer () in
    try
      List.iter (secure_output_string oc) (List.rev !current_simplify_stack)
    with ChannelException -> failwith "Simplify fails on restart!"

let channel_exn_restart s = 
  M.msg_string M.Error ("ChannelException in "^s); 
  restart_simplify ()

let is_substring s subs =
  let reg = Str.regexp subs in
  try ignore(Str.search_forward reg s 0); true
  with Not_found -> false

let rec isValid ic = 
  let line = secure_input_line ic in
  if is_substring line "Bad input" then (M.msg_string M.Error "Simplify poisoned!"; exit 1)
  else if is_substring line "Valid" (* String.contains line 'V'*) then true
  else if is_substring line "Invalid" (* String.contains line 'I'*) then false
  else isValid ic

(********************************************************************************)
(***************** Converting to the Simplify Format ****************************)
(********************************************************************************)


let convert_rel r = 
  match r with Eq -> "EQ" | Ne -> "NEQ" | Le -> "<=" | Lt -> "<" | Gt -> ">" | Ge -> ">="

let convert_op o = 
  match o with Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" | Mod -> "%"

let simplifyToTable = Hashtbl.create 17

let freshSimplifySymbol =
  let n = ref 0 in
  (fun () -> n := !n + 1; "v"^(string_of_int !n))

let convertSymbol x =
  try Hashtbl.find simplifyToTable x with Not_found ->
    let x' = freshSimplifySymbol () in
    let _ = Hashtbl.add simplifyToTable x x' in
    x'

let convertPath path = convertSymbol (Path.unique_name path)

let rec convert_exp e =
  match e with
    PInt i -> if i >= 0 then string_of_int i
               else convert_exp (Binop(PInt(0), Minus, PInt(abs i))) 
  | Var x -> convertPath x
  | Binop (e1,op,e2) -> Printf.sprintf "(%s %s %s)" (convert_op op) (convert_exp e1) (convert_exp e2)
  | FunApp (f,e) -> Printf.sprintf "(%s %s)" (convertSymbol f) (convert_exp (List.hd e)) (* this is incorrect, e is a list *)
  | Field (f, e) -> Printf.sprintf "(SELECT_%s %s)" f (convert_exp e)
  | Proj (n, e) -> Printf.sprintf "(PROJ_%d %s)" n (convert_exp e)

let rec convert_pred p = 
  match p with 
    True -> "(EQ 0 0)"
  | Atom (e1,Predicate.Lt,e2) ->
      convert_pred (Atom (e1, Predicate.Le, Binop(e2,Predicate.Minus,PInt 1)))
  | Atom (e1,r,e2) -> Printf.sprintf "(%s %s %s)" (convert_rel r) (convert_exp e1) (convert_exp e2)
  | Iff _ as iff -> convert_pred (Predicate.expand_iff iff)
  | Not p -> Printf.sprintf "(NOT %s)" (convert_pred p) 
  | And (p1,p2) -> Printf.sprintf "(AND %s %s)" (convert_pred p1) (convert_pred p2)
  | Or (p1,p2) -> Printf.sprintf "(OR %s %s)" (convert_pred p1) (convert_pred p2)
	| _ -> assert false (* Simplify prover is not really working *)


(********************************************************************************)
(**************************** Issuing Queries ***********************************)
(********************************************************************************)

let push pred =
  let s = Printf.sprintf "(BG_PUSH %s) \n" (convert_pred pred) in
  let _ = current_simplify_stack := s :: !current_simplify_stack in
  let _ = simplify_stack_counter := !simplify_stack_counter + 1 in
  let (_,oc) = getServer () in
  try secure_output_string oc s;flush oc ;     
  with ChannelException -> channel_exn_restart "simplify_assume"

let pop () = 
  let _ = 
    if (!simplify_stack_counter = 0) then failwith "bad simplify stack!"
    else simplify_stack_counter := !simplify_stack_counter - 1 in
  let _ = current_simplify_stack := List.tl !current_simplify_stack in
  let (_,oc) = getServer () in
  let s = "(BG_POP)\n" in
  try secure_output_string oc s; flush oc
  with ChannelException -> channel_exn_restart "simplify_pop" 

let valid p = 
  let s = convert_pred p in
  let rec qe flag = 
    let _ = flush stdout; flush stderr in 
    try
      let ic,oc = getServer () in
      secure_output_string oc s;flush oc; isValid ic
    with ChannelException when flag -> failwith "Simplify fails again!" 
       | ChannelException -> restart_simplify (); qe true
       | e -> 
         failwith (Format.fprintf Format.str_formatter
           "Simplify raises %s for %a. Check that Simplify is in your path \n" 
           (Printexc.to_string e) Predicate.pprint p; Format.flush_str_formatter ()) in
  qe false

let reset () = 
  Misc.repeat_fn pop (!simplify_stack_counter)

let num_queries = ref 0
let queries_between_resets = 80000

let implies (p, q) =
  let _ = incr num_queries in
  let _ = if !Clflags.kill_simplify && !num_queries mod queries_between_resets = 0 then restart_simplify () else () in 

  let s = Printf.sprintf "(IMPLIES %s %s)" (convert_pred p) (convert_pred q) in
  let rec qe flag = 
    let _ = flush stdout; flush stderr in 
    try
      let ic,oc = getServer () in
      secure_output_string oc s;flush oc; isValid ic 
    with ChannelException when flag -> failwith "Simplify fails again!" 
       | ChannelException -> restart_simplify (); qe true
       | e -> 
         failwith (Format.fprintf Format.str_formatter
           "Simplify raises %s for %a. Check that Simplify is in your path \n" 
           (Printexc.to_string e) Predicate.pprint p; Format.flush_str_formatter ()) in
    qe false

let print_simplify p q = 
  let ps = convert_pred p in
  let qs = convert_pred q in
  Printf.printf "Simplify query : %s : %s \n" ps qs

end

module Prover = SimplifyProver
