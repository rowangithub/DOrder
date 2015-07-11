open Swig
open Oyicesunsafe
open List

type yices_ast = Yices_ast of c_obj
type yices_expr = Yices_expr of c_obj
type yices_type = Yices_type of c_obj
type yices_var_decl = Yices_var_decl of c_obj
type yices_context = Yices_context of c_obj
type yices_model = Yices_model of c_obj
type yices_var_decl_iterator = Yices_var_decl_iterator of c_obj

exception NoCoercion

val yices_set_verbosity: int -> unit
val yices_version: unit -> string
val yices_set_max_num_conflicts_in_maxsat_iteration: int32 -> unit
val yices_enable_type_checker: int -> unit
val yices_set_maxsat_initial_cost: int64 -> unit
val yices_set_arith_only: int -> unit
val yices_enable_log_file: string -> unit
val yices_mk_context: unit -> yices_context
val yices_del_context: yices_context -> unit
val yices_reset: yices_context -> unit
val yices_dump_context: yices_context -> unit
val yices_push: yices_context -> unit
val yices_pop: yices_context -> unit
val yices_assert: yices_context -> yices_expr -> unit
val yices_inconsistent: yices_context -> int
val yices_check: yices_context -> int
val yices_find_weighted_model: yices_context -> int -> int
val yices_evaluate_in_model: yices_model -> yices_expr -> int
val yices_max_sat: yices_context -> int
val yices_max_sat_cost_leq: yices_context -> int64 -> int
val yices_get_model: yices_context -> yices_model
val yices_display_model: yices_model -> unit
val yices_get_cost_as_double: yices_model -> float
val yices_get_value: yices_model -> yices_var_decl -> int
val yices_get_bitvector_value: yices_model -> yices_var_decl -> int32 -> int array -> int
val yices_mk_true: yices_context -> yices_expr
val yices_mk_false: yices_context -> yices_expr
val yices_mk_bool_var: yices_context -> string -> yices_expr
val yices_mk_fresh_bool_var: yices_context -> yices_expr
val yices_get_var_decl: yices_expr -> yices_var_decl
val yices_mk_bool_var_decl: yices_context -> string -> yices_var_decl
val yices_get_var_decl_name: yices_var_decl -> string
val yices_mk_bool_var_from_decl: yices_context -> yices_var_decl -> yices_expr
val yices_mk_or: yices_context -> yices_expr array -> yices_expr
val yices_mk_and: yices_context -> yices_expr array -> yices_expr
val yices_mk_eq: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_diseq: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_ite: yices_context -> yices_expr -> yices_expr -> yices_expr -> yices_expr
val yices_mk_not: yices_context -> yices_expr -> yices_expr
val yices_mk_type: yices_context -> string -> yices_type
val yices_mk_function_type: yices_context -> yices_type array -> yices_type -> yices_type
val yices_mk_bitvector_type: yices_context -> int32 -> yices_type
val yices_mk_var_decl: yices_context -> string -> yices_type -> yices_var_decl
val yices_get_var_decl_from_name: yices_context -> string -> yices_var_decl
val yices_mk_var_from_decl: yices_context -> yices_var_decl -> yices_expr
val yices_create_var_decl_iterator: yices_context -> yices_var_decl_iterator
val yices_iterator_has_next: yices_var_decl_iterator -> int
val yices_iterator_next: yices_var_decl_iterator -> yices_var_decl
val yices_iterator_reset: yices_var_decl_iterator -> unit
val yices_mk_var_decl: yices_context -> string -> yices_type -> yices_var_decl
val yices_del_iterator: yices_var_decl_iterator -> unit
val yices_mk_var_from_decl: yices_context -> yices_var_decl -> yices_expr
val yices_mk_app: yices_context -> yices_expr -> yices_expr array -> yices_expr
val yices_mk_num: yices_context -> int -> yices_expr
val yices_mk_num_from_string: yices_context -> string -> yices_expr
val yices_mk_sum: yices_context -> yices_expr array -> yices_expr
val yices_mk_sub: yices_context -> yices_expr array -> yices_expr
val yices_mk_mul: yices_context -> yices_expr array -> yices_expr
val yices_mk_lt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_le: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_gt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_ge: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_constant_from_array: yices_context -> int32 -> int array -> yices_expr
val yices_mk_bv_add: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_sub: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_mul: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_minus: yices_context -> yices_expr -> yices_expr
val yices_mk_bv_concat: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_and: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_or: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_xor: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_not: yices_context -> yices_expr -> yices_expr
val yices_mk_bv_extract: yices_context -> int32 -> int32 -> yices_expr -> yices_expr
val yices_mk_bv_sign_extend: yices_context -> yices_expr -> int32 -> yices_expr
val yices_mk_bv_shift_left0: yices_context -> yices_expr -> int32 -> yices_expr
val yices_mk_bv_shift_left1: yices_context -> yices_expr -> int32 -> yices_expr
val yices_mk_bv_shift_right0: yices_context -> yices_expr -> int32 -> yices_expr
val yices_mk_bv_shift_right1: yices_context -> yices_expr -> int32 -> yices_expr
val yices_mk_bv_lt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_le: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_gt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_ge: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_slt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_sle: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_mk_bv_sgt: yices_context -> yices_expr -> yices_expr -> yices_expr
val yices_pp_expr: yices_expr -> unit

val yices_get_int_value: yices_model -> yices_var_decl -> int