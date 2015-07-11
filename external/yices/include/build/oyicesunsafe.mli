open Swig
type c_enum_type = [ 
  `unknown
| `lbool
]
type c_enum_value = [ 
  `Int of int
| `l_false
| `l_undef
| `l_true
]

type c_obj = c_enum_value c_obj_t
val module_name : string

exception BadArgs of string
exception BadMethodName of c_obj * string * string
exception NotObject of c_obj
exception NotEnumType of c_obj
exception LabelNotFromThisEnum of c_obj
exception InvalidDirectorCall of c_obj

val _l_false : c_obj -> Swig.c_obj
val _l_undef : c_obj -> Swig.c_obj
val _l_true : c_obj -> Swig.c_obj
val _yices_set_verbosity : c_obj -> c_obj
val _yices_version : c_obj -> c_obj
val _yices_set_max_num_conflicts_in_maxsat_iteration : c_obj -> c_obj
val _yices_enable_type_checker : c_obj -> c_obj
val _yices_set_max_num_iterations_in_maxsat : c_obj -> c_obj
val _yices_set_maxsat_initial_cost : c_obj -> c_obj
val _yices_set_arith_only : c_obj -> c_obj
val _yices_enable_log_file : c_obj -> c_obj
val _yices_mk_context : c_obj -> c_obj
val _yices_del_context : c_obj -> c_obj
val _yices_reset : c_obj -> c_obj
val _yices_dump_context : c_obj -> c_obj
val _yices_push : c_obj -> c_obj
val _yices_pop : c_obj -> c_obj
val _yices_assert : c_obj -> c_obj
val _yices_assert_weighted : c_obj -> c_obj
val _yices_assert_retractable : c_obj -> c_obj
val _yices_retract : c_obj -> c_obj
val _yices_inconsistent : c_obj -> c_obj
val _yices_check : c_obj -> c_obj
val _yices_find_weighted_model : c_obj -> c_obj
val _yices_evaluate_in_model : c_obj -> c_obj
val _yices_max_sat : c_obj -> c_obj
val _yices_max_sat_cost_leq : c_obj -> c_obj
val _yices_get_model : c_obj -> c_obj
val _yices_display_model : c_obj -> c_obj
val _yices_get_cost : c_obj -> c_obj
val _yices_get_cost_as_double : c_obj -> c_obj
val _yices_get_value : c_obj -> c_obj
val _yices_get_int_value : c_obj -> c_obj
val _yices_get_arith_value : c_obj -> c_obj
val _yices_get_double_value : c_obj -> c_obj
val _yices_get_bitvector_value : c_obj -> c_obj
val _yices_get_assertion_value : c_obj -> c_obj
val _yices_mk_true : c_obj -> c_obj
val _yices_mk_false : c_obj -> c_obj
val _yices_mk_bool_var : c_obj -> c_obj
val _yices_mk_fresh_bool_var : c_obj -> c_obj
val _yices_get_var_decl : c_obj -> c_obj
val _yices_mk_bool_var_decl : c_obj -> c_obj
val _yices_get_var_decl_name : c_obj -> c_obj
val _yices_mk_bool_var_from_decl : c_obj -> c_obj
val _yices_mk_or : c_obj -> c_obj
val _yices_mk_and : c_obj -> c_obj
val _yices_mk_eq : c_obj -> c_obj
val _yices_mk_diseq : c_obj -> c_obj
val _yices_mk_ite : c_obj -> c_obj
val _yices_mk_not : c_obj -> c_obj
val _yices_mk_type : c_obj -> c_obj
val _yices_mk_function_type : c_obj -> c_obj
val _yices_mk_bitvector_type : c_obj -> c_obj
val _yices_mk_tuple_type : c_obj -> c_obj
val _yices_mk_var_decl : c_obj -> c_obj
val _yices_get_var_decl_from_name : c_obj -> c_obj
val _yices_mk_var_from_decl : c_obj -> c_obj
val _yices_create_var_decl_iterator : c_obj -> c_obj
val _yices_iterator_has_next : c_obj -> c_obj
val _yices_iterator_next : c_obj -> c_obj
val _yices_iterator_reset : c_obj -> c_obj
val _yices_del_iterator : c_obj -> c_obj
val _yices_mk_app : c_obj -> c_obj
val _yices_mk_num : c_obj -> c_obj
val _yices_mk_num_from_string : c_obj -> c_obj
val _yices_mk_sum : c_obj -> c_obj
val _yices_mk_sub : c_obj -> c_obj
val _yices_mk_mul : c_obj -> c_obj
val _yices_mk_lt : c_obj -> c_obj
val _yices_mk_le : c_obj -> c_obj
val _yices_mk_gt : c_obj -> c_obj
val _yices_mk_ge : c_obj -> c_obj
val _yices_mk_bv_constant : c_obj -> c_obj
val _yices_mk_bv_constant_from_array : c_obj -> c_obj
val _yices_mk_bv_add : c_obj -> c_obj
val _yices_mk_bv_sub : c_obj -> c_obj
val _yices_mk_bv_mul : c_obj -> c_obj
val _yices_mk_bv_minus : c_obj -> c_obj
val _yices_mk_bv_concat : c_obj -> c_obj
val _yices_mk_bv_and : c_obj -> c_obj
val _yices_mk_bv_or : c_obj -> c_obj
val _yices_mk_bv_xor : c_obj -> c_obj
val _yices_mk_bv_not : c_obj -> c_obj
val _yices_mk_bv_extract : c_obj -> c_obj
val _yices_mk_bv_sign_extend : c_obj -> c_obj
val _yices_mk_bv_shift_left0 : c_obj -> c_obj
val _yices_mk_bv_shift_left1 : c_obj -> c_obj
val _yices_mk_bv_shift_right0 : c_obj -> c_obj
val _yices_mk_bv_shift_right1 : c_obj -> c_obj
val _yices_mk_bv_lt : c_obj -> c_obj
val _yices_mk_bv_le : c_obj -> c_obj
val _yices_mk_bv_gt : c_obj -> c_obj
val _yices_mk_bv_ge : c_obj -> c_obj
val _yices_mk_bv_slt : c_obj -> c_obj
val _yices_mk_bv_sle : c_obj -> c_obj
val _yices_mk_bv_sgt : c_obj -> c_obj
val _yices_pp_expr : c_obj -> c_obj
val enum_to_int : c_enum_type -> c_obj -> Swig.c_obj
val int_to_enum : c_enum_type -> int -> c_obj

  val swig_val : c_enum_type -> c_obj -> Swig.c_obj

