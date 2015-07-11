(*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
*****************************************************************************)
open Query

type var_t = int

type lit_t = int

type boolformula_type_t = Lit | And | Or

type boolformula_t

external literal_of_var : var_t -> lit_t
  = "ocaml_boolformula_lit_from_var"

external var_of_literal : lit_t -> var_t
  = "ocaml_boolformula_var_from_lit"

external literal_complement : lit_t -> lit_t
  = "ocaml_boolformula_lit_complement"

external positive_literal : lit_t -> bool
  = "ocaml_boolformula_positive_lit"

external from_literal : lit_t -> boolformula_t 
  = "ocaml_create_literal"

external create_disjunction : int -> boolformula_t 
  = "ocaml_create_disjunction"

external create_conjunction : int -> boolformula_t 
  = "ocaml_create_conjunction"

external add_boolformula : boolformula_t -> boolformula_t -> boolformula_t
  = "ocaml_add_boolformula"

external set_boolformula : boolformula_t -> int -> boolformula_t -> 
  boolformula_t = "ocaml_set_boolformula"

external get_type : boolformula_t -> boolformula_type_t 
  = "ocaml_get_type"

external get_length : boolformula_t -> int 
  = "ocaml_get_length"

external get_boolformula : boolformula_t -> int -> boolformula_t 
  = "ocaml_get_boolformula"

external get_literal : boolformula_t -> lit_t 
  = "ocaml_get_literal"

external print : boolformula_t -> unit 
  = "ocaml_print"

external num_vars : boolformula_t -> int 
  = "ocaml_num_vars"

external to_cnf : boolformula_t -> int -> boolformula_t 
  = "ocaml_to_cnf"

external copy : boolformula_t -> boolformula_t 
  = "ocaml_copy"

type membership_t = bool array -> mem_result_t

type comembership_t = membership_t

type equivalence_t = int -> boolformula_t -> eq_result_t

external cdnf : int -> string -> string -> boolformula_t option
  = "ocaml_cdnf"

external cdnfx : int -> string -> string -> boolformula_t
  = "ocaml_cdnfx"

external cdnfxx : int -> string -> string -> string -> boolformula_t 
  = "ocaml_cdnfxx"

external cdnfxxx : int -> string -> string -> boolformula_t
  = "ocaml_cdnfxxx"

let session = ref 0

let register_is_member session is_member =
  let is_member_name = "ocaml_is_member_" ^ (string_of_int session) in
  let _ = Callback.register is_member_name is_member in
  is_member_name

let register_is_comember session is_comember =
  let is_comember_name = "ocaml_is_comember_" ^ (string_of_int session) in
  let _ = Callback.register is_comember_name is_comember in
  is_comember_name

let register_is_equivalent session is_equivalent =
  let is_equivalent_name = "ocaml_is_equivalent_" ^ (string_of_int session) in
  let _ = Callback.register is_equivalent_name is_equivalent in
  is_equivalent_name

let go num_vars is_member is_equivalent =
  let _ = incr session in
  let (is_member_name, is_equivalent_name) = 
    register_is_member !session is_member,
    register_is_equivalent !session is_equivalent in
    cdnf num_vars is_member_name is_equivalent_name

let gox num_vars is_member is_equivalent =
  let _ = incr session in
  let (is_member_name, is_equivalent_name) = 
    register_is_member !session is_member,
    register_is_equivalent !session is_equivalent in
    cdnfx num_vars is_member_name is_equivalent_name

let goxx num_vars is_member is_comember is_equivalent =
  let _ = incr session in
  let (is_member_name, is_comember_name, is_equivalent_name) = 
    register_is_member !session is_member,
    register_is_comember !session is_comember,
    register_is_equivalent !session is_equivalent in
    cdnfxx num_vars is_member_name is_comember_name is_equivalent_name

let goxxx num_vars is_member is_equivalent =
  let _ = incr session in
  let (is_member_name, is_equivalent_name) = 
    register_is_member !session is_member,
    register_is_equivalent !session is_equivalent in
    cdnfxxx num_vars is_member_name is_equivalent_name
