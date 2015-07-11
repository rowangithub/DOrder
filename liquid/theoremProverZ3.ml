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

(* This file is part of the Dsolve Project.*)

open Format
open Predef

module P = Predicate

(***************************************************************)
(*module type PROVER = 
  sig
    (* usage: set.valid*.finish *)
    val axiom : F.t Le.t -> Predicate.t -> unit
    val set: F.t Le.t -> P.t list -> bool
    val filter: F.t Le.t -> ('a * P.t) list -> ('a * P.t) list
    val finish: unit -> unit
    val print_stats : Format.formatter -> unit -> unit
    val embed_type : F.t * Parsetree.prover_t -> unit
    val frame_of : Parsetree.prover_t -> F.t
    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list
  end*)
	
module type PROVER = 
  sig
		type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list
		
		val push : Predicate.t -> unit 
		val pop : unit -> unit

    val implies : Predicate.t -> Predicate.t -> bool

    val finish : unit -> unit

    val print_stats : unit -> unit
		
		val model : Predicate.t -> int -> ((Path.t, int) Hashtbl.t) list
		
		val sat : Predicate.t -> bool
		
		val unsatisfiable : Predicate.t -> bool
  end	

module Z3Prover : PROVER = 
 struct

    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list

    type decl = Vbl of Path.t | Fun of string * int | Barrier
    type var_ast = Const of Z3.ast | Bound of int * sort
    
    type z3_instance = { 
      mutable c                 : Z3.context;
      mutable tint              : Z3.sort;
      mutable tbool             : Z3.sort;
      mutable vart              : (decl, var_ast) Hashtbl.t;
      mutable funt              : (decl, Z3.func_decl) Hashtbl.t;
      mutable tydeclt           : (sort, Z3.sort) Hashtbl.t;
      mutable vars      : decl list ;
      mutable count     : int;
      mutable bnd       : int;
      (*mutable frtymap   : (F.t * sort) list;*)
    }
		
   (* stats *)
    let nb_z3_push  = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop   = ref 0
    let nb_z3_set   = ref 0

    let fresh =
      let x = ref 0 in
      (fun v -> incr x; (v^(string_of_int !x)))

(***************************************************************************************)
(********************** Typing ************************************************************)
(***************************************************************************************)

    let bofi = "_BOFI"
    let iofb = "_IOFB"
    let div = "_DIV"
		let skolem = "__skolem"
		let tag = "__tag"

    let builtins = [
            (tag, Func [Int; Int]);
            (div, Func [Int; Int; Int]);
            (iofb, Func [Bool; Int]);
            (bofi, Func [Int; Bool]);
						(skolem, Func [Int; Unint "skolem_constant"]);
						("Reach", Func [Int; Int; Bool]);
						("Link", Func [Int; Int; Int; Int; Int; Bool])
    ]

    let unint = Unint "obj"

    (*let init_frtymap = [
      (Builtins.uInt, Int); 
      (Builtins.uBool, Bool);
    ]*)

    let type_to_string t =
      let rec t_rec = function
        | Int -> "int"
        | Bool -> "bool"
        | Unint s -> s
        | Func ts -> "(func " ^ String.concat " " (List.map t_rec ts) ^ ")"
        | Array (s, t) -> "[| " ^ t_rec s ^ "; " ^ t_rec t ^ " |]" in
      t_rec t

    let ast_type_to_string me a =
      Z3.sort_to_string me.c (Z3.get_sort me.c a)

    let dump_ast_type me a =
      printf "@[z3%s@]@." (ast_type_to_string me a)

    let dump_ast me a =
      printf "@[%s@]@." (Z3.ast_to_string me.c a)

    let dump_decls me =
      printf "Vars:@.";
      List.iter (function 
				| Vbl s -> 
					(let ast = Hashtbl.find me.vart (Vbl s) in
					match ast with
						| Const ast -> printf "%s --> %s@." (Path.unique_name s) (Z3.ast_to_string me.c ast)
						| _ -> printf "%s@." (Path.unique_name s))
				| Barrier -> printf "----@." 
				| _ -> ()) me.vars;
      printf "@."
   
    (*let rec frame_to_type me = function
      | F.Farrow (_, t1, t2, _) -> Func (collapse me t1 t2)
      | fr -> snd (List.find (fun (fr', _) -> F.same_shape fr fr') me.frtymap)

    and collapse me t1 t2 =
      (try frame_to_type me t1 with Not_found -> unint)
      :: (match t2 with 
          | F.Farrow (_, t1, t2) -> collapse me t1 t2 
          | _ -> try [frame_to_type me t2] with Not_found -> [unint])
                          
    let rec type_to_frame me = function
      | Func (t :: []) -> type_to_frame me t
      | Func (t :: ts) ->
        (try
          List.assoc t (Misc.list_assoc_flip me.frtymap)
        with Not_found -> F.Farrow(F.fresh_binder (), type_to_frame me t, type_to_frame me (Func ts)))
      | t -> List.assoc t (Misc.list_assoc_flip me.frtymap) *)

    let rec z3VarType me = function
      | Int -> me.tint
      | Bool -> me.tbool
      | Array _ -> assert false
      | Unint _ -> me.tint
      | Func _ -> z3VarType me (Unint ("fun"))

    let z3VarType me t =
      Misc.do_memo me.tydeclt (fun () -> z3VarType me t) () t

    (*let rec transl_type me = function
      | Parsetree.Pprover_abs s ->
          (match s with 
          | "int" -> Int
          | "bool" -> Bool
          | s -> Unint s)
      | Parsetree.Pprover_array (t, t') ->
          Array (transl_type me t, transl_type me t')
      | Parsetree.Pprover_fun ts ->
          Func (List.map (transl_type me) ts)*)

    let z3ArgTypes me = function 
      | Func ts -> (match (List.rev_map (z3VarType me) ts) with
                      | x :: [] -> ([], x)
                      | x :: xs -> (List.rev xs, x)
                      | [] -> assert false)
      | _ -> assert false

    let getVarType me s (*env*) = Int
      (*let fr = try (Le.find s (*env*)) with
                 Not_found -> let p = Path.unique_name s in
                   (eprintf "@[Warning:@ type@ of@ %s@ not@ found@ at@ TP@]@." p; assert false) in
      try frame_to_type me fr
        with Not_found -> unint*)

		let is_prefix prefix str = 
			Common.str_contains str prefix
			
    let is_select p = is_prefix "SELECT_" p || is_prefix "PROJ_" p
    (*let select = Path.mk_persistent "SELECT"*)
    let select_type = Func [Int; Int]
					
 
    let getFunType me p k (*env*) =
      if is_select (p) then select_type
      else if List.mem_assoc p builtins then
				List.assoc p builtins
			else 
				let t = Int in
        let	tl = Array.make k t in
				Func ((Array.to_list tl) @ [t])
				(*try 
        (*with Not_found -> try frame_to_type me (Le.find p (*env*)) *)
          with Not_found -> printf "@[Warning:@ could@ not@ type@ function@ %s@ in@ tpz3@]" (Path.unique_name p); assert false*)

(***************************************************************************************)
(********************** Vars ***********************************************************)
(***************************************************************************************)
		let do_memo memo f args key = 
  		try Hashtbl.find memo key with Not_found ->
    		let rv = f args in
    		let _ = Hashtbl.replace memo key rv in
    		rv

    let z3Var_memo (*env*) me s =
      do_memo me.vart
      (fun () -> 
        let t = getVarType me s (*env*) in
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Const (Z3.mk_const me.c sym (z3VarType me t)) in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s)

    let z3Var (*env*) me s =
      match z3Var_memo (*env*) me s with
          Const v -> v
        | Bound (b, t) -> Z3.mk_bound me.c (me.bnd - b) (z3VarType me t)

    let z3Bind me p t =
      me.bnd <- me.bnd + 1; Hashtbl.replace me.vart (Vbl p) (Bound (me.bnd, t)); me.vars <- (Vbl p) :: me.vars;
      Z3.mk_string_symbol me.c (fresh "z3b")

(***************************************************************************************)
(********************** Funs ***********************************************************)
(***************************************************************************************)

    let z3Fun (*env*) me p t k = 
      do_memo me.funt
      (fun () ->
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let (ts, ret) = z3ArgTypes me t in
        let rv  = Z3.mk_func_decl me.c sym (Array.of_list ts) ret in
        me.vars <- (Fun (p,k))::me.vars; rv) 
      () (Fun (p,k))

    let rec cast (*env*) me ast (t, t') =
      if (t, t') = ("bool", "int") then z3App (*env*) me iofb [ast] else
      if (t, t') = ("int", "bool") then z3App (*env*) me bofi [ast] else
        assert false

    and z3Cast (*env*) me = function
      | (a :: sa, f :: fs) -> 
        let (t, t') = (Z3.get_sort me.c a, z3VarType me f) in
        let (st, st') = (Z3.sort_to_string me.c t, Z3.sort_to_string me.c t') in
        let t = if st = st' then a else (cast (*env*) me a (st, st')) in
          t :: (z3Cast (*env*) me (sa, fs))
      | ([], x :: []) -> []
      | ([], []) -> []
      | _ -> assert false

    and z3App (*env*) me p zes =
      let k   = List.length zes in
      let ft  = match getFunType me p k(*env*) with 
				| Func ts -> ts 
				| f -> failwith (sprintf "%s@ ::@ %s" ( p) (type_to_string f)) in
      let cf  = z3Fun (*env*) me p (Func ft) k in
      let zes = z3Cast (*env*) me (zes, ft) in
      Z3.mk_app me.c cf (Array.of_list zes)

    and z3AppBuiltin (*env*) me fes aes =
      z3Cast (*env*) me (List.map (z3Exp (*env*) me) aes, fes) 

    and z3AppRel (*env*) me mk e1 e2 =
      match z3AppBuiltin (*env*) me [Int; Int] [e1; e2] with
        [a1; a2] -> mk me.c a1 a2
      | _ -> assert false

(***************************************************************************************)
(********************** Pred/Expr Transl ************************************************************)
(***************************************************************************************)

    and z3Exp (*env*) me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var (*env*) me s
      | P.FunApp (f,es)         -> z3App (*env*) me f (List.map (z3Exp (*env*) me) es)
      | P.Binop (e1,P.Plus,e2)  ->
          Z3.mk_add me.c (Array.map (z3Exp (*env*) me) [|e1; e2|])
      | P.Binop (e1,P.Minus,e2) ->
          Z3.mk_sub me.c (Array.map (z3Exp (*env*) me) [|e1; e2|])
      | P.Binop (e1,P.Times,e2) ->
          Z3.mk_mul me.c (Array.map (z3Exp (*env*) me) [|e1; e2|])
      | P.Binop (e1,P.Div,e2)   -> 
					Z3.mk_div me.c (z3Exp me e1) (z3Exp me e2)
					(*z3App (*env*) me div (List.map (z3Exp (*env*) me) [e1;e2]) *) 
			| P.Binop (e1,P.Mod,e2)   -> 
					Z3.mk_mod me.c (z3Exp me e1) (z3Exp me e2)
      | P.Field (f, e)          -> 
					let f = (Printf.sprintf "SELECT_%s" f) in
					z3App (*env*) me f [(z3Exp (*env*) me e)]
          (** REQUIRES: disjoint intra-module field names *)
			| Predicate.Proj (n, e)   -> 
					let f = (Printf.sprintf "PROJ_%d" n) in
					z3App (*env*) me f [(z3Exp (*env*) me e)]
      (*| P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred (*env*) me e1) (z3Exp (*env*) me e2) (z3Exp (*env*) me e3) *)

    and z3Pred (*env*) me p = 
     try
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (z3Pred (*env*) me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred (*env*) me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred (*env*) me) [|p1;p2|])
      (*| P.Implies (p1, p2) -> Z3.mk_implies me.c (z3Pred (*env*) me p1) (z3Pred (*env*) me p2)*)
      | P.Iff (p, q) as iff -> (*Z3.mk_iff me.c (z3Pred (*env*) me p) (z3Pred (*env*) me q)*)
				z3Pred (*env*) me (Predicate.expand_iff iff)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) ->
          (*Z3.mk_eq me.c (z3Exp (*env*) me e1) (z3Exp (*env*) me e2)*)
          z3AppRel (*env*) me Z3.mk_eq e1 e2
      | P.Atom (e1,P.Ne,e2) ->
          Z3.mk_distinct me.c (Array.of_list (z3AppBuiltin (*env*) me [Int; Int] [e1; e2]))
      | P.Atom (e1,P.Gt,e2) ->
          (*Z3.mk_gt me.c (z3Exp (*env*) me e1) (z3Exp (*env*) me e2)*)
          z3AppRel (*env*) me Z3.mk_gt e1 e2
      | P.Atom (e1,P.Ge,e2) ->
          (*Z3.mk_ge me.c (z3Exp (*env*) me e1) (z3Exp (*env*) me e2)*)
          z3AppRel (*env*) me Z3.mk_ge e1 e2
      | P.Atom (e1,P.Lt,e2) ->
          (*Z3.mk_lt me.c (z3Exp (*env*) me e1) (z3Exp (*env*) me e2)*)
          z3AppRel (*env*) me Z3.mk_lt e1 e2
      | P.Atom (e1,P.Le,e2) ->
          (*Z3.mk_le me.c (z3Exp (*env*) me e1) (z3Exp (*env*) me e2)*)
          z3AppRel (*env*) me Z3.mk_le e1 e2
			(* We should already replace the syntactic sugar Reach with meaninful Link *)
			| Predicate.Reach (d, u) -> 
					(*let res_type = me.tbool in 
					let arg_type = me.t in
					let	arg_tl = Array.make 2 arg_type in
					let ftype = Func (arg_tl @ [res_type]) in*)
					let args = [d; u] in 
					let f = "Reach" in
					z3App (*env*) me f (List.map (fun arg -> z3Exp (*env*) me arg) args)
			| Predicate.Link (d, c, f, u, v) ->
					(*let res_type = Y.yices_mk_type me.c "bool" in
					let arg_type = me.t in
          let	arg_tl = Array.make 5 arg_type in
					let ftype = Y.yices_mk_function_type me.c arg_tl res_type in*)
					let args = [d; 
											Predicate.Var (Path.Pident (Ident.create_persistent c)); 
											Predicate.PInt f; 
											u; v] in
          let f = "Link" in
					z3App (*env*) me f (List.map (fun arg -> z3Exp (*env*) me arg) args)
			| Predicate.Forall (ps, p') -> 
          let ts = List.map (fun _ -> Int) ps in
          mk_quantifier Z3.mk_forall (*env*) me ps ts p'
      (*| P.Exists (ps, q) ->
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_exists (*env*) me ps ts q *)
      | Predicate.Bool e -> 
          (* shady hack *)
          (*let a = z3Exp (*env*) me e in
          let t = ast_type_to_string me a in
          if t = "int" then cast (*env*) me a ("int", "bool") else a*)
					z3Pred me (P.Atom (P.FunApp (tag, [e]),P.Eq,P.PInt 1))
     with Failure s -> printf "%s: %a@." s P.pprint p; raise (Failure s)

    and mk_quantifier mk (*env*) me ps ts q =
      let args = qargs me ps ts in
      let rv = mk me.c 0 [||] (qtypes me ts) args (z3Pred (*env*) me q) in
      me.bnd <- me.bnd - (List.length ps); rv

    and qargs me ps ts = 
      Array.of_list (List.map (fun (p, t) -> z3Bind me p t) (List.combine ps ts))

    and qtypes me ts = Array.of_list (List.map (z3VarType me) ts)  

    let z3Preds (*env*) me ps =
      let ps' = List.map (z3Pred (*env*) me) ps in
      Z3.mk_and me.c (Array.of_list ps')

(***************************************************************************************)
(********************** Low Level Interface ************************************************************)
(***************************************************************************************)
		let me = 
      let c = Z3.mk_context_x [|("MODEL", "false"); (*"MODEL_PARTIAL", "true"*)|] in
      (* types *)
      let tint = Z3.mk_int_sort c in
      let tbool = Z3.mk_bool_sort c in
      (* memo tables *)
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      let tydeclt = Hashtbl.create 37 in
      { c = c; tint = tint; tbool = tbool; tydeclt = tydeclt; (*frtymap = init_frtymap;*)
        vart = vart; funt = funt; vars = []; count = 0; bnd = 0}
		
		let unsat () =
			let temp = (Bstats.time "Z3 unsat" Z3.check me.c) in
      let rv =  temp = Z3.L_FALSE in
      rv

		let assert_axiom me p =
      let _ = Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p in
      let _ = Common.cprintf 5 "@[%s@]@." (Z3.ast_to_string me.c p) in
        if unsat () then failwith "Background theory is inconsistent!"
				
		let rec vpop (cs,s) =
      match s with [] -> (cs,s)
      | h::t when h = Barrier -> (cs,t)
      | h::t -> vpop (h::cs,t)		

		let remove_decl d = 
      match d with 
      | Barrier -> Common.asserts "TheoremProverZ3.remove_decl" false
      | Vbl _ -> Hashtbl.remove me.vart d 
      | Fun _ -> Hashtbl.remove me.funt d


		let push p =
      me.count <- me.count + 1;
      let p' = Bstats.time "Z3 mk pred" (z3Pred me) p in
      let _ = me.vars <- Barrier :: me.vars in
      let _ = Bstats.time "Z3 stackpush" Z3.push me.c in
      Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p' 
      
    let pop () =
      me.count <- me.count - 1;
      let (cs,vars') = vpop ([],me.vars) in
			let _ = me.vars <- vars' in
			let _ = List.iter (remove_decl) cs in
      Bstats.time "popping" Z3.pop me.c 1

    let reset () =
      Misc.repeat_fn pop me.count;
			Z3.del_context me.c;
		
			let c = Z3.mk_context_x [|("MODEL", "false"); (*"smt.macro-finder","true"*)|] in
			me.c <- c;
      (* types *)
      me.tint <- Z3.mk_int_sort c;
      me.tbool <- Z3.mk_bool_sort c;
      (* memo tables *)
      me.vart <- Hashtbl.create 37 ;
      me.funt <- Hashtbl.create 37 ;
      me.tydeclt <- Hashtbl.create 37;
			me.vars <- [];
			(*me.frtymap <- init_frtymap;*)
			me.count <- 0;
			me.bnd <- 0

    let valid p =
      if unsat () then true else 
        let _ = push (Predicate.Not p) in
        let rv = unsat () in
        let _ = pop () in rv
    
    let implies p =
      let _ = incr nb_z3_push; Bstats.time "Z3 push" push p in
      fun q -> 
				(*let _ = Format.fprintf Format.std_formatter "p: %a => q: %a@."
					Predicate.pprint' p Predicate.pprint' q in*)
				let result = Bstats.time "Z3 valid" valid q in
				((*Format.fprintf Format.std_formatter "verify result = %b@." result;*) result)

    let finish () = 
      Bstats.time "Z3 pop" pop (); 
      assert (me.count = 0)
			
		let sat p = 
			let _ = push p in
			let result = Z3.check me.c in
			let _ = pop () in
			(*let _ = Format.fprintf Format.std_formatter "result = %d@." result in*)
			(result	= Z3.L_TRUE)
			
		let unsatisfiable p = 	
			let _ = push p in
			let result = Z3.check me.c in
			let _ = pop () in
			(*let _ = Format.fprintf Format.std_formatter "result = %d@." result in*)
			(result	= Z3.L_FALSE)

    (*let unsat me =
      let _ = incr nb_z3_unsat in
      let rv = (Bstats.time "Z3 unsat" Z3.check me.c) = Z3.L_FALSE in
      rv

    let assert_axiom me p =
      let _ = Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p in
      let _ = Common.cprintf Common.ol_axioms "@[%s@]@." (Z3.ast_to_string me.c p) in
        if unsat me then failwith "Background theory is inconsistent!"

    let rec vpop (cs,s) =
      match s with 
      | [] -> (cs,s)
      | Barrier :: t -> (cs,t)
      | h :: t -> vpop (h::cs,t) 

    let remove_decl me d = 
      match d with 
      | Barrier -> Common.asserts "TheoremProverZ3.remove_decl" false
      | Vbl _ -> Hashtbl.remove me.vart d 
      | Fun _ -> Hashtbl.remove me.funt d

    let prep_preds (*env*) me ps =
      let ps = List.rev_map (z3Pred (*env*) me) ps in
      let _ = me.vars <- Barrier :: me.vars in
      let _ = Z3.push me.c in
        ps

    let push me ps =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      let _  = Z3.push me.c in
        List.iter (fun p -> Z3.assert_cnstr me.c p) ps

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
        Z3.pop me.c 1 

    let valid me p =
      let _ = push me [(Z3.mk_not me.c p)] in
      let rv = unsat me in
      let _ = pop me in
        rv*)

(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    (*let clean_decls () =
      let (cs,vars') = vpop ([],me.vars) in
      let _ = me.vars <- vars' in
      let _ = List.iter (remove_decl me) cs in ()

    let set (*env*) ps =
      let _   = Hashtbl.remove me.vart (Vbl Common.qual_test_var) in
      let ps  = prep_preds (*env*) me ps in
      let _   = push me ps in
        unsat me

    let filter (*env*) ps =
      let rv =
        let ps = List.rev_map (fun (q, p) -> (z3Pred (*env*) me p, (q, p))) ps in
        List.filter (fun (p, _) -> valid me p) ps in
      let _ = pop me in
      let _ = clean_decls () in
        snd (List.split rv)

    let finish () =
      pop me

    let axiom (*env*) p =
      assert_axiom me (z3Pred (*env*) me p)

    let embed_type (fr, t) =
      me.frtymap <- (fr, transl_type me t) :: me.frtymap

    let frame_of pt =
      try
        type_to_frame me (transl_type me pt)
      with Not_found -> raise (Failure (Common.prover_t_to_s pt))

    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_z3_set !nb_z3_push !nb_z3_pop !nb_z3_unsat

    let _ = 
      let (x, y) = Misc.map_pair Path.mk_ident ("x", "y") in
      let bol = Parsetree.Pprover_abs "bool" in
      let itn = Parsetree.Pprover_abs "int" in
      let func s x = P.FunApp(s, [P.Var x]) in
        axiom Le.empty
          (P.Forall ([(x, bol)], P.Iff(P.Atom(func iofb x, P.Eq, P.PInt(1)), P.Boolexp(P.Var x))));
        axiom Le.empty 
          (P.Forall ([(x, itn)], P.Iff(P.Boolexp(func bofi x), P.Atom(P.Var x, P.Eq, P.PInt(1)))));*)
						
		let model p number =
			let _ = push p in
	    match Z3.check_and_get_model me.c with
	    | (Z3.L_TRUE, m) ->
	      begin
					let vars = Predicate.vars p in
					let vars = Common.remove_duplicates vars in
					let valuetbl = Hashtbl.create 7 in
					let _ = List.iter (fun var ->
						match Hashtbl.find me.vart (Vbl var) with
							| Const ast -> 
								let (b, value) = Z3.eval me.c m ast in
								if b then
									(try 
										Hashtbl.replace valuetbl var 
										(let str = Z3.get_numeral_string me.c value in
										(int_of_string str))
	    						with _ -> ())
								else ()
							| Bound _ -> assert false 
					) vars in
					(pop (); [valuetbl])
	      end;
	    | (_,_) ->
	      (pop (); [])
				
		let print_stats () = 
    	Printf.printf "Z3 pushes = %d \n" !nb_z3_push			
end
    
module Prover = Z3Prover