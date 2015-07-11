module Misc = QpMisc

(*************************************************************************)
(************************ Datatypes **************************************)
(*************************************************************************)

type id = int

type value = Atom | Fun of string * id list

type inst = { 
  valuet        : (id,value) Hashtbl.t ; 
  parentt       : (id,id) Hashtbl.t ;
  watcht        : (id, id) Hashtbl.t;
  sizet         : (id, int) Hashtbl.t;
  mutable wkl   : (id * id) list;
  mutable mcb   : (id -> id -> unit) option;
}

let get_value me i = Hashtbl.find me.valuet i
let set_value me i = Hashtbl.replace me.valuet i
let get_parent me i = Hashtbl.find me.parentt i
let set_parent me i = Hashtbl.replace me.parentt i
let get_watch me i = Hashtbl.find_all me.watcht i
let set_watch me i = Hashtbl.add me.watcht i
let get_size me i = Hashtbl.find me.sizet i
let set_size me i = Hashtbl.add me.sizet i

let fresh_id = 
  let r = ref 0 in
  (fun () -> incr r; !r)

let new_node me v =
  let i = fresh_id () in
  set_value me i v;
  set_parent me i i;
  set_size me i 1;
  i

(*************************************************************************)
(************************ API ********************************************)
(*************************************************************************)

let new_instance () = 
  { valuet      = Hashtbl.create 17;
    parentt     = Hashtbl.create 17;
    watcht      = Hashtbl.create 17;
    sizet       = Hashtbl.create 17;
    wkl         = [];
    mcb         = None;} 

let set_merge_callback me f = 
  me.mcb <- Some f

let rec get_rep me i = 
  let i' = get_parent me i in
  if i' = i then i else
    let i'' = get_rep me i' in
    set_parent me i i''; i''

let check_equal me i i' =
  get_rep me i = get_rep me i'

let new_atom me = 
  new_node me Atom

let rec new_application me f js =
  let i  = new_node me (Fun (f,js)) in
  let js = List.map (get_rep me) js in
  match js with [] -> i | j::_ -> 
    List.iter (fun j -> set_watch me j i) js;
    List.iter (check_cong me i) (get_watch me j);
    grind me; i

and assert_equal me i i' = 
  me.wkl <- (i,i')::me.wkl;
  grind me

and check_cong me i i' = 
  match (get_value me i, get_value me i') with
    Fun (f,is), Fun (f',is') -> 
      if f = f' && List.for_all2 (check_equal me) is is' then assert_equal me i i'
  | _ -> Misc.assert_false "check_cong"

and merge me i j =
  let i = get_rep me i in
  let j = get_rep me j in
  if i = j then () else 
    (let (i,j) = if get_size me i < get_size me j then (i,j) else (j,i) in
     set_parent me i j;
     set_size me j (get_size me i + get_size me j);
     Misc.iter_cross (check_cong me) (get_watch me i) (get_watch me j);
     List.iter (set_watch me i) (get_watch me j);
     (match me.mcb with None -> () | Some f -> f i j))

and grind me = 
  match me.wkl with [] -> () | (i,j)::t -> 
    me.wkl <- t; merge me i j; grind me

