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
		val push : Predicate.t -> unit 
		val pop : unit -> unit
    (*
    (* push p: tell prover to assume fact p *)
    val push : Predicate.t -> unit 
    
    (* pop () : tell prover to un-assume last assumed fact *)
    val pop : unit -> unit 
    
    (* reset (): tell prover to remove all assumed facts *)
    val reset : unit -> unit 
    
    (* valid p : do the currently assumed facts imply p ? *)
    val valid : Predicate.t -> bool
    *)

    (* implies p q = true iff predicate p (provably) implies predicate q *)
    val implies : Predicate.t -> Predicate.t -> bool

    val finish : unit -> unit

    val print_stats : unit -> unit
		
		val model : Predicate.t -> int -> ((Path.t, int) Hashtbl.t) list
		
		val sat : Predicate.t -> bool
		
		val unsatisfiable : Predicate.t -> bool
		
  end


module Y = Oyices

module YicesProver : PROVER = 
  struct
    type yices_instance = { 
      mutable c : Y.yices_context;
      mutable t : Y.yices_type;
      mutable f : Y.yices_type;
      mutable binop: Y.yices_type; (* for uninterp ops *)
      mutable d : (string,Y.yices_var_decl) Hashtbl.t;
      mutable ds : string list ;
      mutable count : int;
      mutable i : int;
      mutable consistent: bool;
    }

    let nb_yices_push = ref 0

    let barrier = "0" 

    let yicesVar me s ty =
      let decl = 
        Misc.do_memo me.d
        (fun () -> 
          let rv = Y.yices_mk_var_decl me.c s ty in
            me.ds <- s::me.ds;rv) () s in
      Y.yices_mk_var_from_decl me.c decl

    let rec isconst = function
	Predicate.PInt(i) -> true
      | _ -> false

    let rec yicesExp me e =
      match e with 
        Predicate.PInt i -> Y.yices_mk_num me.c i 
      | Predicate.Var s -> yicesVar me (Path.unique_name s) me.t
      | Predicate.FunApp (f,e) -> 
          (*let (fn, e') = (yicesVar me f me.f, List.map (yicesExp me) e) in Y.yices_mk_app me.c fn (Array.of_list e')*)
					let t = me.t in
          let	tl = Array.make (List.length e) t in
          let ftype = Y.yices_mk_function_type me.c tl t in
          let (func, e') = (yicesVar me f ftype, List.map (yicesExp me) e) in
          Y.yices_mk_app me.c func (Array.of_list e')
      | Predicate.Binop (e1,op,e2) ->
          let es' = Array.map (yicesExp me) [|e1;e2|] in
          (match op with
						| Predicate.Plus  -> Y.yices_mk_sum me.c es'
           	| Predicate.Minus -> Y.yices_mk_sub me.c es'
           	| Predicate.Times ->
							if (isconst e1) || (isconst e2) then
					 			Y.yices_mk_mul me.c es'
				    	else
					 			let (fd, e1, e2) = (yicesVar me "_MUL" me.binop, yicesExp me e1, yicesExp me e2) in
					   		Y.yices_mk_app me.c fd [|e1; e2|]
						| Predicate.Div ->
							if (isconst e2) then
								let (fd, e1, e2) = (Y.yices_mk_var_from_decl me.c (Y.yices_get_var_decl_from_name me.c "div"), 
																yicesExp me e1, yicesExp me e2) in
						 		Y.yices_mk_app me.c fd [|e1; e2|]	
							else 
								let (fd, e1, e2) = (yicesVar me "_DIV" me.binop, yicesExp me e1, yicesExp me e2) in
					 			Y.yices_mk_app me.c fd [|e1; e2|]
				 		| Predicate.Mod ->
							if (isconst e2) then
						 		let (fd, e1, e2) = (Y.yices_mk_var_from_decl me.c (Y.yices_get_var_decl_from_name me.c "mod"), 
																yicesExp me e1, yicesExp me e2) in
						 		Y.yices_mk_app me.c fd [|e1; e2|]
							else 
								let (fd, e1, e2) = (yicesVar me "_MOD" me.binop, yicesExp me e1, yicesExp me e2) in
					 			Y.yices_mk_app me.c fd [|e1; e2|])
      | Predicate.Field (f, e) ->
          (* pmr: this crucially depends on not having two fields with the same name in the
             same module *)
          let (fn, e') = (yicesVar me (Printf.sprintf "SELECT_%s" f) me.f, yicesExp me e) in
            Y.yices_mk_app me.c fn [|e'|]
      | Predicate.Proj (n, e) ->
          let (fn, e') = (yicesVar me (Printf.sprintf "PROJ_%d" n) me.f, yicesExp me e) in
            Y.yices_mk_app me.c fn [|e'|]

    let rec yicesPred me p = 
      match p with 
        Predicate.True -> Y.yices_mk_true me.c
      | Predicate.Not p' -> Y.yices_mk_not me.c (yicesPred me p')
      | Predicate.And (p1,p2) -> Y.yices_mk_and me.c (Array.map (yicesPred me) [|p1;p2|])
      | Predicate.Or (p1,p2) -> Y.yices_mk_or me.c (Array.map (yicesPred me) [|p1;p2|])
      | Predicate.Iff _ as iff -> yicesPred me (Predicate.expand_iff iff)
      | Predicate.Atom (e1,Predicate.Lt,e2) ->
          yicesPred me (Atom (e1, Predicate.Le, Binop(e2,Predicate.Minus,PInt 1)))
    (* RJ: why not this ?
     * | P.Atom (e1,P.Gt,e2) -> 
          yicesPred me (Atom (e2, P.Le, Binop(e1,P.Minus,PInt 1))) *)
			(*| Predicate.Atom (Predicate.Binop (x, Predicate.Mod, y), Predicate.Eq, z)
			| Predicate.Atom (z, Predicate.Eq, Predicate.Binop (x, Predicate.Mod, y)) -> 
				let p = 
						((((x >=. PInt 0) =>. (PInt 0 <=. z)) &&.
            ((y <. PInt 0) ||. (y >. PInt 0))) &&.
            ((y >. PInt 0) =>. (z <. y))) &&. 										
						(z ==. (x -- (y *- (x /- y)))) in
				 yicesPred me p	*)
      | Predicate.Atom (e1,br,e2) ->
          let e1' = yicesExp me e1 in
          let e2' = yicesExp me e2 in
          (match br with 
             Predicate.Eq -> Y.yices_mk_eq me.c e1' e2' 
           | Predicate.Ne -> Y.yices_mk_diseq me.c e1' e2'
           | Predicate.Gt -> Y.yices_mk_gt me.c e1' e2'
           | Predicate.Ge -> Y.yices_mk_ge me.c e1' e2'
           | Predicate.Lt -> Y.yices_mk_lt me.c e1' e2'
           | Predicate.Le -> Y.yices_mk_le me.c e1' e2')
			(* We should already replace the syntactic sugar Reach with meaninful Link *)
			| Predicate.Reach (d, u) -> 
					let res_type = Y.yices_mk_type me.c "bool" in 
					let arg_type = me.t in
					let	arg_tl = Array.make 2 arg_type in
					let args = [|d; u|] in 
					let ftype = Y.yices_mk_function_type me.c arg_tl res_type in
          let (func, e') = (yicesVar me "Reach" ftype, Array.map (yicesExp me) args) in
          Y.yices_mk_app me.c func e'
			| Predicate.Link (d, c, f, u, v) ->
					let res_type = Y.yices_mk_type me.c "bool" in
					let arg_type = me.t in
          let	arg_tl = Array.make 5 arg_type in
					let args = [|d; Predicate.Var (Path.Pident (Ident.create_persistent c)); Predicate.PInt f; u; v|] in
          let ftype = Y.yices_mk_function_type me.c arg_tl res_type in
          let (func, e') = (yicesVar me "Link" ftype, Array.map (yicesExp me) args) in
          Y.yices_mk_app me.c func e'
			| Predicate.Forall (ps, p') -> 
				(Format.fprintf Format.std_formatter "%a@." Predicate.pprint' p; assert false)	
			| Predicate.Bool b -> (Format.fprintf Format.std_formatter "%a@." Predicate.pprint_pexpr' b; assert false)	

    let me = 
      let c = Y.yices_mk_context () in
      let _ = if !Clflags.log_queries then Y.yices_enable_log_file "yices.log" else () in
      let t = Y.yices_mk_type c "int" in
      let binop = Y.yices_mk_function_type c [| t; t |] t in
      let f = Y.yices_mk_function_type c [| t |] t in
      let d = Hashtbl.create 37 in
        { c = c; t = t; f = f; binop = binop; d = d; 
          ds = []; count = 0; i = 0; consistent = true}

    let push p =
      me.count <- me.count + 1;
      if (Bstats.time "Yices consistency" Y.yices_inconsistent me.c) = 1 
      then me.i <- me.i + 1 else
        let p' = Bstats.time "Yices mk pred" (yicesPred me) p in
        let _ = me.ds <- barrier :: me.ds in
        let _ = Bstats.time "Yices stackpush" Y.yices_push me.c in
        Bstats.time "Yices assert" (Y.yices_assert me.c) p' 
      
    let rec vpop (cs,s) =
      match s with [] -> (cs,s)
      | h::t when h = barrier -> (cs,t)
      | h::t -> vpop (h::cs,t)

    let pop () =
      me.count <- me.count - 1;
      if me.i > 0 then me.i <- me.i - 1 else
        let (cs,ds') = vpop ([],me.ds) in
	let _ = me.ds <- ds' in
	let _ = List.iter (Hashtbl.remove me.d) cs in
        Bstats.time "popping" Y.yices_pop me.c

    let reset () =
      Misc.repeat_fn pop me.count;
      Y.yices_reset me.c;
      me.t <- Y.yices_mk_type me.c "int";
      me.binop <- Y.yices_mk_function_type me.c [| me.t; me.t |] me.t;
      me.f <- Y.yices_mk_function_type me.c [| me.t |] me.t;
      me.d <- Hashtbl.create 37;
      me.ds <- [];
      me.count <- 0;
      me.i <- 0

    let unsat () =
			let temp = (Bstats.time "Yices unsat" Y.yices_check me.c) in
      let rv =  temp = -1 in
      rv

    let valid p =
      if unsat () then true else 
        let _ = push (Predicate.Not p) in
        let rv = unsat () in
        let _ = pop () in rv
    
    let implies p =
			let _ = Format.fprintf Format.std_formatter "push p = %a@." Predicate.pprint' p in
      let _ = incr nb_yices_push; Bstats.time "Yices push" push p in
      fun q -> 
				let _ = Format.fprintf Format.std_formatter "p: %a => q: %a@."
					Predicate.pprint' p Predicate.pprint' q in
				let result = Bstats.time "Yices valid" valid q in
				(Format.fprintf Format.std_formatter "verify result = %b@." result; result)

    let finish () = 
      Bstats.time "YI pop" pop (); 
      assert (me.count = 0)
			
		let sat p = 
			let _ = push p in
			let result = Y.yices_check me.c in
			let _ = pop () in
			(*let _ = Format.fprintf Format.std_formatter "result = %d@." result in*)
			(result	= 1)
			
		let unsatisfiable p = 	
			let _ = push p in
			let result = Y.yices_check me.c in
			let _ = pop () in
			(*let _ = Format.fprintf Format.std_formatter "result = %d@." result in*)
			(result	= -1)
			
		let model p number = 
			let vars = Predicate.vars p in
			let vars = Common.remove_duplicates vars in
			let _ = push p in
			let result = Y.yices_check me.c in
			if (result = 1) then
				let model = Y.yices_get_model me.c in
				(*let _ = Y.yices_display_model model in*)
				let valuetbl = Hashtbl.create 7 in
				let _ = List.iter (fun var ->
					let varstr = Path.unique_name var in
					let decl = 
						try Hashtbl.find me.d varstr 
						with _ -> assert false in 
					(*let value = 
						try Y.yices_get_int_value model decl 
						with _ -> (Format.fprintf Format.std_formatter "Cannot find value for %s in %a\n" varstr Predicate.pprint p;
							pop ();
							assert false) in
					Hashtbl.replace valuetbl var value*)
					try 
						let value = Y.yices_get_int_value model decl in
						Hashtbl.replace valuetbl var value
					with _ -> ()
					) vars in
				(pop (); [valuetbl])
			else ((*Format.fprintf Format.std_formatter "result = %d\n" result;*) pop (); [])
		(*let model p number = 
			let number = 1 in
			let vars = Predicate.vars p in
			let vars = Common.remove_duplicates vars in
			let rec modelsolve p index valuetbls = 
				if (index < number) then
					let _ = push p in
					let result = Y.yices_check me.c in
					if (result = 1) then
						let _ = Format.fprintf Format.std_formatter "modelsolve index %d number %d \n" index number in
						let model = Y.yices_get_model me.c in
						let _ = Y.yices_display_model model in
						let valuetbl = Hashtbl.create 7 in
						let _ = List.iter (fun var ->
							let varstr = Path.unique_name var in
							let decl = 
								try Hashtbl.find me.d varstr 
								with _ -> assert false in 
							let value = 
								try Y.yices_get_int_value model decl 
								with _ -> (Format.fprintf Format.std_formatter "Cannot find value for %s in %a\n" varstr Predicate.pprint p;
									assert false) in
							let _ = Printf.printf "%s = %d\n" varstr value in
							Hashtbl.replace valuetbl var value
							) vars in
						(pop (); modelsolve p (index+1) (valuetbls @ [valuetbl]))
					else (Printf.printf "Cannot retrieve model\n"; pop (); valuetbls) 
				else valuetbls in
			modelsolve p 0 []*)
  
(*
  let implies p q = 
      let _ = (* Bstats.time "pushing p" *) push p in
      let rv = (* Bstats.time "validating" *) valid q in
      let _ = (* Bstats.time "popping" *) pop () in
      rv 
  let finish () = ()
*)
  
  let print_stats () = 
    Printf.printf "Yices pushes = %d \n" !nb_yices_push

end

module Prover = YicesProver
