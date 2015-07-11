(*****************************************************************************)
(****************************** Type Definitions *****************************)
(*****************************************************************************)
module Misc = QpMisc
open QpDag
module IntHash = Misc.IntHash
module Vector = QpVector
module ExprHash = Expression.Hash
module PredHash = Predicate.Hash
module SymbolHashStruct = struct
  type t = Symbol.symbol
  let equal x y = (x = y)
  let hash = Hashtbl.hash
end
module SymbolHash = Hashtbl.Make(SymbolHashStruct)
open QpArch

module Prover : PROVER =
  struct 

exception Inconsistent

type variable = {
  def : expression;
  id : int;
}

type uif = {
  fdef : string;
}

type instance = { 
  new_fact : predicate -> unit; (* callback to register prover for new fact *)
  vars : variable Vector.vector; (* the variables *)
  uifs : uif Vector.vector; (* the uninterpreted functions *)
  exprVart : int ExprHash.t;
  symbolFunt: int SymbolHash.t;
  cong : Cong.inst; 
  mutable consistent : bool;
  mutable merges : (Cong.id * Cong.id) list;
  mutable diseqs : (int * int) list;
}

(*****************************************************************************)
(****************************** Constructors  ********************************)
(*****************************************************************************)

let fake_not_uif = {fdef = ""}

let fake_var = {def = zero; id = 0} 

let add_var me v =
  Vector.resize me.vars (v.id+1); 
  Vector.set me.vars v.id v;
  v.id

let get_var me x = 
  Vector.get me.vars x

let rec make_var me e =
  let n = 
    match e with
      | Application(s,l),_ ->
 	  Cong.new_application me.cong s (List.map (find_var me) l)
      | _ -> Cong.new_atom me.cong in
  { id = n; def = e; }

and find_var me e =
  try ExprHash.find me.exprVart e with Not_found ->
    (match e with
    | Application(_,l),_ -> List.iter (fun x -> ignore(find_var me x)) l
    | Variable _,_ -> ()
    | _ -> find_var_subexps me e);
    let n = add_var me (make_var me e) in
    ExprHash.add me.exprVart e n; n

and find_var_subexps me e =
  match e with
      Application(_,_),_ 
    | Variable _,_ -> ignore (find_var me e)
    | Sum es,_ -> List.iter (find_var_subexps me) es
    | Coeff (_,e),_ -> find_var_subexps me e
    | Constant _,_ -> ()

let do_merges me = 
  List.iter 
    (fun (i,j) -> me.new_fact (pEquality ((get_var me i).def,(get_var me j).def)))
    (List.rev (me.merges));
  me.merges <- []

let is_eq me i j = 
  Cong.check_equal me.cong i j

let check_diseq me (i,j) = 
  if is_eq me i j then me.consistent <- false

let check_consistency me =
  List.iter (check_diseq me) me.diseqs;
  do_merges me

let add_diseq me xv yv = 
  me.diseqs <- (xv,yv)::me.diseqs;
  check_diseq me (xv,yv);
  if is_eq me xv yv then me.consistent <- false

(**************************************************************************)
(************************* API functions **********************************)
(**************************************************************************)

(* API *)
let new_instance new_fact =
  let me =
    { new_fact = new_fact;
      vars = Vector.make 0 fake_var;
      uifs = Vector.make 0 fake_not_uif;
      cong = Cong.new_instance (); 
      exprVart = ExprHash.create 251;
      symbolFunt = SymbolHash.create 251;
      consistent = true;
      merges = [];
      diseqs = [];
    } in
  Cong.set_merge_callback me.cong (fun i j -> me.merges <- (i,j)::me.merges);
  me

(* API *)  
let push me = function 
    | Equality (x,y),_ ->
	Cong.assert_equal me.cong (find_var me x) (find_var me y);
	check_consistency me
    | (Not((Equality (x,y),_) as equ),_) as diseq ->
         add_diseq me (find_var me x) (find_var me y)
    | False,_ -> me.consistent <- false
    | _ -> ()

(* API *)  
let is_consistent me = 
  me.consistent

(* API *)  
let is_valid me = function 
  | Equality (x,y),_ ->
      is_eq me (find_var me x) (find_var me y)
  | (Not((Equality (x,y),_) as equ),_) as diseq ->
      let xv = find_var me x in
      let yv = find_var me y in
      List.exists 
        (fun (i,j) -> (is_eq me xv i && is_eq me yv j) || (is_eq me xv i && is_eq me yv j)) 
        me.diseqs
  | _ when not me.consistent -> true
  | True,_ -> true
  | _ -> false

        (* {{{ 
  let f = find_uif me fake_uif in
  let i = Cong.new_application me.cong f [xv] in
  let j = Cong.new_application me.cong f [yv] in
  IntHash.add me.diseqHash i equ;
  IntHash.add me.diseqHash j equ;
  check_consistency me;}}} *)
    (* {{{
    | Leq(x,y),_  | Not(Leq(x,y),_),_ -> 
        find_var_subexps me x; find_var_subexps me y; 
        check_consistency me
    | _ -> () }}} *)


  end

