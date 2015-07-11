(** Difference Constraint Solver: 
  * A difference constraint is of the form:
  * x - y <= c
  * x <= c can be encoded as: x - x0 <= c
  * x >= c can be encoded as: x0 - x <= c
  * x - y < c can be encoded as: x - y <= c-1 *)

module Q = Queue
module Misc = QpMisc
open QpArch
open QpDag
module EH = Expression.Hash

module Prover : PROVER = 
  struct
(***************************************************************************)
(******************** Type Definitions *************************************)
(***************************************************************************)

type var  = int

type edge = var * var

type diffcons = var * var * int (* x - y <= c *)

type instance = {
  new_fact      : predicate -> unit;
  mutable vars  : var list ;
  succt         : (var,var) Hashtbl.t;
  predt         : (var,var) Hashtbl.t;
  dist          : (edge,int) Hashtbl.t;
  exprVart      : var EH.t; 
  varExprt      : (var,expression) Hashtbl.t;
  wkl           : diffcons Q.t;
  mutable cst   : bool;
}


(**************************************************************************)
(************************ Get/Set  ****************************************)
(**************************************************************************)

let get_dist me x y =
  try Some(Hashtbl.find me.dist (x,y)) 
  with Not_found -> None

let get_preds me x =
  let xs' = Hashtbl.find_all me.predt x in
  let ds' = Misc.map_partial (fun x' -> get_dist me x' x) xs' in
  let _   = assert (List.length xs' = List.length ds') in
  List.combine xs' ds'

let get_succs me x = 
  let xs' = Hashtbl.find_all me.succt x in
  let ds' = Misc.map_partial (fun x' -> get_dist me x x') xs' in
  let _   = assert (List.length xs' = List.length ds') in
  List.combine xs' ds'

let get_expr me id = 
  Hashtbl.find me.varExprt id

let get_var = 
  let vr = ref 0 in
  fun me e ->
    try EH.find me.exprVart e with Not_found ->
      let id = incr vr; !vr in
      let _ = Hashtbl.add me.varExprt id e in
      let _ = EH.add me.exprVart e id in
      id

let set_dist me x y c =
  let _ = match get_dist me x y with Some _ -> () | None ->
    (Hashtbl.add me.succt x y; Hashtbl.add me.predt y x) in
  Hashtbl.replace me.dist (x,y) c

(**************************************************************************)
(************************ Check/Propagate *********************************)
(**************************************************************************)

let is_known me (x,y,c) = 
  match get_dist me y x with 
  | Some c' -> c' <= c
  | None -> false 
  
let new_equality me x y = 
  me.new_fact (pEquality (get_expr me x, get_expr me y))

let propagate me (y,x,c) =
  if x = y && c < 0 then me.cst <- false else
    (set_dist me x y c; 
     (if c = 0 && is_known me (x,y,0) then new_equality me x y);
     List.iter (fun (x',c') -> Q.push (y,x',c+c') me.wkl) (get_preds me x);
     List.iter (fun (y',c') -> Q.push (y',x,c+c') me.wkl) (get_succs me y))

let rec grind me = 
  if not (Q.is_empty me.wkl) then
    let dc = Q.pop me.wkl in
    let _ = if not (is_known me dc) then propagate me dc in
    grind me

(**********************************************************************************)
(************************ Internalize *********************************************)
(**********************************************************************************)
(* type expression[term] = 
     Constant _ | Variable _ | Application _ | Coeff (_,expression[term]) *)

let push_coeff c e = 
  match e with
  | Application(_,_),_ | Variable _,_ -> eCoeff (Constant.Int c,e) 
  | Constant (Constant.Int c'),_ -> eConstant (Constant.Int (c*c')) 
  | Coeff (Constant.Int c',e'),_ -> eCoeff (Constant.Int (c*c'),e')

let rec expr_to_sum_terms e =
  match e with
  | Application(_,_),_ | Variable _,_ -> [eCoeff (Constant.Int 1,e)] 
  | Constant _,_ -> [e] 
  | Sum es,_ -> Misc.flap expr_to_sum_terms es 
  | Coeff (Constant.Int c',e'),_ -> List.map (push_coeff c') (expr_to_sum_terms e')
  | _ -> Misc.assert_false "match failure in expr_to_sum_terms"

let get_coeff t e =
  try EH.find t e with Not_found -> 0

let set_coeff t e c =
  EH.replace t e c

let add_term t e =
  let (c,x) = 
    match e with Constant (Constant.Int c),_ -> (c,p_one) 
    | Coeff (Constant.Int c,x),_ -> (c,x) in
  set_coeff t x (c + (get_coeff t x))

let termo_to_term = 
  function Some x -> x | _ -> zero 

let consto_to_const = 
  function Some c -> c | _ -> 0

exception NotDC 
let expr_to_dc e =
  let t = EH.create 17 in
  let _ = List.iter (add_term t) (expr_to_sum_terms e) in
  try 
    let (xo,yo,co) = 
      EH.fold 
        (fun e c -> function 
          | (xo,yo,None) when e = p_one -> (xo,yo,Some c)
          | (None,yo,co) when c = 1     -> (Some e,yo,co)
          | (xo,None,co) when c = -1    -> (xo,Some e,co)
          | _ -> raise NotDC) t (None,None,None) in
    Some (termo_to_term xo, termo_to_term yo, consto_to_const co)
  with NotDC -> None

(* {{{ 

let add_term x y = 
  match (x,y) with
  | _ when x = zero -> y
  | _ when y = zero -> x
  | ((Constant (Constant.Int cx),_),(Constant (Constant.Int cy),_)) 
    -> eConstant (Constant.Int (cx+cy))
  | ((Coeff (Constant.Int cx, ex),_), (Coeff (Constant.Int cy, ey),_)) 
    when ex=ey -> eCoeff (Constant.Int (cx+cy),ex)
  | _ -> raise SumFail
}}} *)

let internalize_leqz me e =
  match expr_to_dc e with
  | Some (ex, ey, c) ->
      let x = get_var me ex in
      let y = get_var me ey in
      Some (x,y,c)
  | _ -> None 

(**********************************************************************************)
(************************ API functions *******************************************)
(**********************************************************************************)

(* API *)
let new_instance new_fact = {
  new_fact   = new_fact;
  vars       = [];
  succt      = Hashtbl.create 251;
  predt      = Hashtbl.create 251;
  dist       = Hashtbl.create 251;
  exprVart   = EH.create 251;
  varExprt   = Hashtbl.create 251;
  wkl        = Q.create ();
  cst        = true ;
}

(* API *)
let is_consistent me = 
  me.cst

let is_valid_leqz me e = 
  match internalize_leqz me e with
  | Some dc -> is_known me dc
  | _ -> false

let eMinus e1 e2 = 
  eSum [e1; eCoeff (Constant.Int (-1),e2)]

(* API *)
let is_valid me = function 
  | Equality (x,y),_ ->
      List.for_all (is_valid_leqz me) [(eMinus x y); (eMinus y x)]
  | Leq (x,y),_ ->
      is_valid_leqz me (eMinus x y)
  | (Not((Leq (x,y),_)),_) -> 
      is_valid_leqz me (eMinus (eSum [y; p_one]) x)
  | _ -> false 

let push_leqz me e =
  match internalize_leqz me e with 
  | Some dc -> Q.push dc me.wkl 
  | _ -> () 

(* API *)
let rec push me = function
  | Equality (x,y),_ ->
      List.iter (push_leqz me) [(eMinus x y); (eMinus y x)]
  | Leq (x,y),_ ->
      push_leqz me (eMinus x y)
  | (Not((Leq (x,y),_)),_) -> 
      push_leqz me (eMinus (eSum [y; p_one]) x)
  | _ -> ()

  end

