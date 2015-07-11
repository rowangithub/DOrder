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
let module_name = "oyicesunsafe"

exception BadArgs of string
exception BadMethodName of c_obj * string * string
exception NotObject of c_obj
exception NotEnumType of c_obj
exception LabelNotFromThisEnum of c_obj
exception InvalidDirectorCall of c_obj

let _ = Callback.register "lbool_marker" (`lbool)
external _l_false : c_obj -> Swig.c_obj = "_wrap_l_false" 
external _l_undef : c_obj -> Swig.c_obj = "_wrap_l_undef" 
external _l_true : c_obj -> Swig.c_obj = "_wrap_l_true" 
external _yices_set_verbosity_f : c_obj list -> c_obj list = "_wrap_yices_set_verbosityoyicesunsafe" ;;
let _yices_set_verbosity arg = match _yices_set_verbosity_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_version_f : c_obj list -> c_obj list = "_wrap_yices_versionoyicesunsafe" ;;
let _yices_version arg = match _yices_version_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_set_max_num_conflicts_in_maxsat_iteration_f : c_obj list -> c_obj list = "_wrap_yices_set_max_num_conflicts_in_maxsat_iterationoyicesunsafe" ;;
let _yices_set_max_num_conflicts_in_maxsat_iteration arg = match _yices_set_max_num_conflicts_in_maxsat_iteration_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_enable_type_checker_f : c_obj list -> c_obj list = "_wrap_yices_enable_type_checkeroyicesunsafe" ;;
let _yices_enable_type_checker arg = match _yices_enable_type_checker_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_set_max_num_iterations_in_maxsat_f : c_obj list -> c_obj list = "_wrap_yices_set_max_num_iterations_in_maxsatoyicesunsafe" ;;
let _yices_set_max_num_iterations_in_maxsat arg = match _yices_set_max_num_iterations_in_maxsat_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_set_maxsat_initial_cost_f : c_obj list -> c_obj list = "_wrap_yices_set_maxsat_initial_costoyicesunsafe" ;;
let _yices_set_maxsat_initial_cost arg = match _yices_set_maxsat_initial_cost_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_set_arith_only_f : c_obj list -> c_obj list = "_wrap_yices_set_arith_onlyoyicesunsafe" ;;
let _yices_set_arith_only arg = match _yices_set_arith_only_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_enable_log_file_f : c_obj list -> c_obj list = "_wrap_yices_enable_log_fileoyicesunsafe" ;;
let _yices_enable_log_file arg = match _yices_enable_log_file_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_context_f : c_obj list -> c_obj list = "_wrap_yices_mk_contextoyicesunsafe" ;;
let _yices_mk_context arg = match _yices_mk_context_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_del_context_f : c_obj list -> c_obj list = "_wrap_yices_del_contextoyicesunsafe" ;;
let _yices_del_context arg = match _yices_del_context_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_reset_f : c_obj list -> c_obj list = "_wrap_yices_resetoyicesunsafe" ;;
let _yices_reset arg = match _yices_reset_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_dump_context_f : c_obj list -> c_obj list = "_wrap_yices_dump_contextoyicesunsafe" ;;
let _yices_dump_context arg = match _yices_dump_context_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_push_f : c_obj list -> c_obj list = "_wrap_yices_pushoyicesunsafe" ;;
let _yices_push arg = match _yices_push_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_pop_f : c_obj list -> c_obj list = "_wrap_yices_popoyicesunsafe" ;;
let _yices_pop arg = match _yices_pop_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_assert_f : c_obj list -> c_obj list = "_wrap_yices_assertoyicesunsafe" ;;
let _yices_assert arg = match _yices_assert_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_assert_weighted_f : c_obj list -> c_obj list = "_wrap_yices_assert_weightedoyicesunsafe" ;;
let _yices_assert_weighted arg = match _yices_assert_weighted_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_assert_retractable_f : c_obj list -> c_obj list = "_wrap_yices_assert_retractableoyicesunsafe" ;;
let _yices_assert_retractable arg = match _yices_assert_retractable_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_retract_f : c_obj list -> c_obj list = "_wrap_yices_retractoyicesunsafe" ;;
let _yices_retract arg = match _yices_retract_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_inconsistent_f : c_obj list -> c_obj list = "_wrap_yices_inconsistentoyicesunsafe" ;;
let _yices_inconsistent arg = match _yices_inconsistent_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_check_f : c_obj list -> c_obj list = "_wrap_yices_checkoyicesunsafe" ;;
let _yices_check arg = match _yices_check_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_find_weighted_model_f : c_obj list -> c_obj list = "_wrap_yices_find_weighted_modeloyicesunsafe" ;;
let _yices_find_weighted_model arg = match _yices_find_weighted_model_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_evaluate_in_model_f : c_obj list -> c_obj list = "_wrap_yices_evaluate_in_modeloyicesunsafe" ;;
let _yices_evaluate_in_model arg = match _yices_evaluate_in_model_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_max_sat_f : c_obj list -> c_obj list = "_wrap_yices_max_satoyicesunsafe" ;;
let _yices_max_sat arg = match _yices_max_sat_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_max_sat_cost_leq_f : c_obj list -> c_obj list = "_wrap_yices_max_sat_cost_leqoyicesunsafe" ;;
let _yices_max_sat_cost_leq arg = match _yices_max_sat_cost_leq_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_model_f : c_obj list -> c_obj list = "_wrap_yices_get_modeloyicesunsafe" ;;
let _yices_get_model arg = match _yices_get_model_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_display_model_f : c_obj list -> c_obj list = "_wrap_yices_display_modeloyicesunsafe" ;;
let _yices_display_model arg = match _yices_display_model_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_cost_f : c_obj list -> c_obj list = "_wrap_yices_get_costoyicesunsafe" ;;
let _yices_get_cost arg = match _yices_get_cost_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_cost_as_double_f : c_obj list -> c_obj list = "_wrap_yices_get_cost_as_doubleoyicesunsafe" ;;
let _yices_get_cost_as_double arg = match _yices_get_cost_as_double_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_value_f : c_obj list -> c_obj list = "_wrap_yices_get_valueoyicesunsafe" ;;
let _yices_get_value arg = match _yices_get_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_int_value_f : c_obj list -> c_obj list = "_wrap_yices_get_int_valueoyicesunsafe" ;;
let _yices_get_int_value arg = match _yices_get_int_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_arith_value_f : c_obj list -> c_obj list = "_wrap_yices_get_arith_valueoyicesunsafe" ;;
let _yices_get_arith_value arg = match _yices_get_arith_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_double_value_f : c_obj list -> c_obj list = "_wrap_yices_get_double_valueoyicesunsafe" ;;
let _yices_get_double_value arg = match _yices_get_double_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_bitvector_value_f : c_obj list -> c_obj list = "_wrap_yices_get_bitvector_valueoyicesunsafe" ;;
let _yices_get_bitvector_value arg = match _yices_get_bitvector_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_assertion_value_f : c_obj list -> c_obj list = "_wrap_yices_get_assertion_valueoyicesunsafe" ;;
let _yices_get_assertion_value arg = match _yices_get_assertion_value_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_true_f : c_obj list -> c_obj list = "_wrap_yices_mk_trueoyicesunsafe" ;;
let _yices_mk_true arg = match _yices_mk_true_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_false_f : c_obj list -> c_obj list = "_wrap_yices_mk_falseoyicesunsafe" ;;
let _yices_mk_false arg = match _yices_mk_false_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bool_var_f : c_obj list -> c_obj list = "_wrap_yices_mk_bool_varoyicesunsafe" ;;
let _yices_mk_bool_var arg = match _yices_mk_bool_var_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_fresh_bool_var_f : c_obj list -> c_obj list = "_wrap_yices_mk_fresh_bool_varoyicesunsafe" ;;
let _yices_mk_fresh_bool_var arg = match _yices_mk_fresh_bool_var_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_var_decl_f : c_obj list -> c_obj list = "_wrap_yices_get_var_decloyicesunsafe" ;;
let _yices_get_var_decl arg = match _yices_get_var_decl_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bool_var_decl_f : c_obj list -> c_obj list = "_wrap_yices_mk_bool_var_decloyicesunsafe" ;;
let _yices_mk_bool_var_decl arg = match _yices_mk_bool_var_decl_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_var_decl_name_f : c_obj list -> c_obj list = "_wrap_yices_get_var_decl_nameoyicesunsafe" ;;
let _yices_get_var_decl_name arg = match _yices_get_var_decl_name_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bool_var_from_decl_f : c_obj list -> c_obj list = "_wrap_yices_mk_bool_var_from_decloyicesunsafe" ;;
let _yices_mk_bool_var_from_decl arg = match _yices_mk_bool_var_from_decl_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_or_f : c_obj list -> c_obj list = "_wrap_yices_mk_oroyicesunsafe" ;;
let _yices_mk_or arg = match _yices_mk_or_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_and_f : c_obj list -> c_obj list = "_wrap_yices_mk_andoyicesunsafe" ;;
let _yices_mk_and arg = match _yices_mk_and_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_eq_f : c_obj list -> c_obj list = "_wrap_yices_mk_eqoyicesunsafe" ;;
let _yices_mk_eq arg = match _yices_mk_eq_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_diseq_f : c_obj list -> c_obj list = "_wrap_yices_mk_diseqoyicesunsafe" ;;
let _yices_mk_diseq arg = match _yices_mk_diseq_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_ite_f : c_obj list -> c_obj list = "_wrap_yices_mk_iteoyicesunsafe" ;;
let _yices_mk_ite arg = match _yices_mk_ite_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_not_f : c_obj list -> c_obj list = "_wrap_yices_mk_notoyicesunsafe" ;;
let _yices_mk_not arg = match _yices_mk_not_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_type_f : c_obj list -> c_obj list = "_wrap_yices_mk_typeoyicesunsafe" ;;
let _yices_mk_type arg = match _yices_mk_type_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_function_type_f : c_obj list -> c_obj list = "_wrap_yices_mk_function_typeoyicesunsafe" ;;
let _yices_mk_function_type arg = match _yices_mk_function_type_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bitvector_type_f : c_obj list -> c_obj list = "_wrap_yices_mk_bitvector_typeoyicesunsafe" ;;
let _yices_mk_bitvector_type arg = match _yices_mk_bitvector_type_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_tuple_type_f : c_obj list -> c_obj list = "_wrap_yices_mk_tuple_typeoyicesunsafe" ;;
let _yices_mk_tuple_type arg = match _yices_mk_tuple_type_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_var_decl_f : c_obj list -> c_obj list = "_wrap_yices_mk_var_decloyicesunsafe" ;;
let _yices_mk_var_decl arg = match _yices_mk_var_decl_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_get_var_decl_from_name_f : c_obj list -> c_obj list = "_wrap_yices_get_var_decl_from_nameoyicesunsafe" ;;
let _yices_get_var_decl_from_name arg = match _yices_get_var_decl_from_name_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_var_from_decl_f : c_obj list -> c_obj list = "_wrap_yices_mk_var_from_decloyicesunsafe" ;;
let _yices_mk_var_from_decl arg = match _yices_mk_var_from_decl_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_create_var_decl_iterator_f : c_obj list -> c_obj list = "_wrap_yices_create_var_decl_iteratoroyicesunsafe" ;;
let _yices_create_var_decl_iterator arg = match _yices_create_var_decl_iterator_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_iterator_has_next_f : c_obj list -> c_obj list = "_wrap_yices_iterator_has_nextoyicesunsafe" ;;
let _yices_iterator_has_next arg = match _yices_iterator_has_next_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_iterator_next_f : c_obj list -> c_obj list = "_wrap_yices_iterator_nextoyicesunsafe" ;;
let _yices_iterator_next arg = match _yices_iterator_next_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_iterator_reset_f : c_obj list -> c_obj list = "_wrap_yices_iterator_resetoyicesunsafe" ;;
let _yices_iterator_reset arg = match _yices_iterator_reset_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_del_iterator_f : c_obj list -> c_obj list = "_wrap_yices_del_iteratoroyicesunsafe" ;;
let _yices_del_iterator arg = match _yices_del_iterator_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_app_f : c_obj list -> c_obj list = "_wrap_yices_mk_appoyicesunsafe" ;;
let _yices_mk_app arg = match _yices_mk_app_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_num_f : c_obj list -> c_obj list = "_wrap_yices_mk_numoyicesunsafe" ;;
let _yices_mk_num arg = match _yices_mk_num_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_num_from_string_f : c_obj list -> c_obj list = "_wrap_yices_mk_num_from_stringoyicesunsafe" ;;
let _yices_mk_num_from_string arg = match _yices_mk_num_from_string_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_sum_f : c_obj list -> c_obj list = "_wrap_yices_mk_sumoyicesunsafe" ;;
let _yices_mk_sum arg = match _yices_mk_sum_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_sub_f : c_obj list -> c_obj list = "_wrap_yices_mk_suboyicesunsafe" ;;
let _yices_mk_sub arg = match _yices_mk_sub_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_mul_f : c_obj list -> c_obj list = "_wrap_yices_mk_muloyicesunsafe" ;;
let _yices_mk_mul arg = match _yices_mk_mul_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_lt_f : c_obj list -> c_obj list = "_wrap_yices_mk_ltoyicesunsafe" ;;
let _yices_mk_lt arg = match _yices_mk_lt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_le_f : c_obj list -> c_obj list = "_wrap_yices_mk_leoyicesunsafe" ;;
let _yices_mk_le arg = match _yices_mk_le_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_gt_f : c_obj list -> c_obj list = "_wrap_yices_mk_gtoyicesunsafe" ;;
let _yices_mk_gt arg = match _yices_mk_gt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_ge_f : c_obj list -> c_obj list = "_wrap_yices_mk_geoyicesunsafe" ;;
let _yices_mk_ge arg = match _yices_mk_ge_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_constant_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_constantoyicesunsafe" ;;
let _yices_mk_bv_constant arg = match _yices_mk_bv_constant_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_constant_from_array_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_constant_from_arrayoyicesunsafe" ;;
let _yices_mk_bv_constant_from_array arg = match _yices_mk_bv_constant_from_array_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_add_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_addoyicesunsafe" ;;
let _yices_mk_bv_add arg = match _yices_mk_bv_add_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_sub_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_suboyicesunsafe" ;;
let _yices_mk_bv_sub arg = match _yices_mk_bv_sub_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_mul_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_muloyicesunsafe" ;;
let _yices_mk_bv_mul arg = match _yices_mk_bv_mul_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_minus_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_minusoyicesunsafe" ;;
let _yices_mk_bv_minus arg = match _yices_mk_bv_minus_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_concat_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_concatoyicesunsafe" ;;
let _yices_mk_bv_concat arg = match _yices_mk_bv_concat_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_and_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_andoyicesunsafe" ;;
let _yices_mk_bv_and arg = match _yices_mk_bv_and_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_or_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_oroyicesunsafe" ;;
let _yices_mk_bv_or arg = match _yices_mk_bv_or_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_xor_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_xoroyicesunsafe" ;;
let _yices_mk_bv_xor arg = match _yices_mk_bv_xor_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_not_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_notoyicesunsafe" ;;
let _yices_mk_bv_not arg = match _yices_mk_bv_not_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_extract_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_extractoyicesunsafe" ;;
let _yices_mk_bv_extract arg = match _yices_mk_bv_extract_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_sign_extend_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_sign_extendoyicesunsafe" ;;
let _yices_mk_bv_sign_extend arg = match _yices_mk_bv_sign_extend_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_shift_left0_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_shift_left0oyicesunsafe" ;;
let _yices_mk_bv_shift_left0 arg = match _yices_mk_bv_shift_left0_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_shift_left1_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_shift_left1oyicesunsafe" ;;
let _yices_mk_bv_shift_left1 arg = match _yices_mk_bv_shift_left1_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_shift_right0_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_shift_right0oyicesunsafe" ;;
let _yices_mk_bv_shift_right0 arg = match _yices_mk_bv_shift_right0_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_shift_right1_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_shift_right1oyicesunsafe" ;;
let _yices_mk_bv_shift_right1 arg = match _yices_mk_bv_shift_right1_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_lt_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_ltoyicesunsafe" ;;
let _yices_mk_bv_lt arg = match _yices_mk_bv_lt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_le_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_leoyicesunsafe" ;;
let _yices_mk_bv_le arg = match _yices_mk_bv_le_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_gt_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_gtoyicesunsafe" ;;
let _yices_mk_bv_gt arg = match _yices_mk_bv_gt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_ge_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_geoyicesunsafe" ;;
let _yices_mk_bv_ge arg = match _yices_mk_bv_ge_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_slt_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_sltoyicesunsafe" ;;
let _yices_mk_bv_slt arg = match _yices_mk_bv_slt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_sle_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_sleoyicesunsafe" ;;
let _yices_mk_bv_sle arg = match _yices_mk_bv_sle_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_mk_bv_sgt_f : c_obj list -> c_obj list = "_wrap_yices_mk_bv_sgtoyicesunsafe" ;;
let _yices_mk_bv_sgt arg = match _yices_mk_bv_sgt_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external _yices_pp_expr_f : c_obj list -> c_obj list = "_wrap_yices_pp_exproyicesunsafe" ;;
let _yices_pp_expr arg = match _yices_pp_expr_f (fnhelper arg) with
  [] -> C_void
| [x] -> (if false then Gc.finalise 
  (fun x -> ignore ((invoke x) "~" C_void)) x) ; x
| lst -> C_list lst ;;
external f_init : unit -> unit = "f_oyicesunsafe_init" ;;
let _ = f_init ()
let enum_to_int x (v : c_obj) =
   match v with
     C_enum _y ->
     (let y = _y in match (x : c_enum_type) with
       `unknown ->          (match y with
           `Int x -> (Swig.C_int x)
           | _ -> raise (LabelNotFromThisEnum v))
| `lbool -> (match y with
 | `l_false -> _l_false C_void
 | `l_undef -> _l_undef C_void
 | `l_true -> _l_true C_void
| `Int x -> Swig.C_int x
| _ -> raise (LabelNotFromThisEnum v))
) | _ -> (C_int (get_int v))
let _ = Callback.register "oyicesunsafe_enum_to_int" enum_to_int
let int_to_enum x y =
    match (x : c_enum_type) with
      `unknown -> C_enum (`Int y)
| `lbool -> C_enum (
 if y = (get_int (_l_false C_void)) then `l_false else
 if y = (get_int (_l_undef C_void)) then `l_undef else
 if y = (get_int (_l_true C_void)) then `l_true else
`Int y)
let _ = Callback.register "oyicesunsafe_int_to_enum" int_to_enum

  let rec swig_val t v = 
    match v with
        C_enum e -> enum_to_int t v
      | C_list l -> Swig.C_list (List.map (swig_val t) l)
      | C_array a -> Swig.C_array (Array.map (swig_val t) a)
      | _ -> Obj.magic v

