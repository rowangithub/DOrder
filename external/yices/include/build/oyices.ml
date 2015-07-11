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

let yices_set_verbosity l =
    let __ret = _yices_set_verbosity (C_list [(C_int l)]) in
        ()

let yices_version _ =
    let __ret = _yices_version (C_list []) in
        (match __ret with
  C_string ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_set_max_num_conflicts_in_maxsat_iteration n =
    let __ret = _yices_set_max_num_conflicts_in_maxsat_iteration (C_list [(C_int32 n)]) in
        ()

let yices_enable_type_checker flag =
    let __ret = _yices_enable_type_checker (C_list [(C_int flag)]) in
        ()

let yices_set_maxsat_initial_cost c =
    let __ret = _yices_set_maxsat_initial_cost (C_list [(C_int64 c)]) in
        ()

let yices_set_arith_only flag =
    let __ret = _yices_set_arith_only (C_list [(C_int flag)]) in
        ()

let yices_enable_log_file file_name =
    let __ret = _yices_enable_log_file (C_list [(C_string file_name)]) in
        ()

let yices_mk_context _ =
    let __ret = _yices_mk_context (C_list []) in
        (Yices_context __ret)

let yices_del_context ctx =
    let __ret = _yices_del_context (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        ()

let yices_reset ctx =
    let __ret = _yices_reset (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        ()

let yices_dump_context ctx =
    let __ret = _yices_dump_context (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        ()

let yices_push ctx =
    let __ret = _yices_push (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        ()

let yices_pop ctx =
    let __ret = _yices_pop (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        ()

let yices_assert ctx expr =
    let __ret = _yices_assert (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match expr with
  Yices_expr _expr -> _expr
| _ -> raise NoCoercion)]) in
        ()

let yices_inconsistent ctx =
    let __ret = _yices_inconsistent (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (match __ret with
  C_int ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_check ctx =
    let __ret = _yices_check (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_find_weighted_model ctx random =
    let __ret = _yices_find_weighted_model (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_int random)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_evaluate_in_model m e =
    let __ret = _yices_evaluate_in_model (C_list [(match m with
  Yices_model _m -> _m
| _ -> raise NoCoercion); (match e with
  Yices_expr _e -> _e
| _ -> raise NoCoercion)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_max_sat ctx =
    let __ret = _yices_max_sat (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_max_sat_cost_leq c max_cost =
    let __ret = _yices_max_sat_cost_leq (C_list [(match c with
  Yices_context _c -> _c
| _ -> raise NoCoercion); (C_int64 max_cost)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_get_model ctx =
    let __ret = _yices_get_model (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (Yices_model __ret)

let yices_display_model m =
    let __ret = _yices_display_model (C_list [(match m with
  Yices_model _m -> _m
| _ -> raise NoCoercion)]) in
        ()

let yices_get_cost_as_double m =
    let __ret = _yices_get_cost_as_double (C_list [(match m with
  Yices_model _m -> _m
| _ -> raise NoCoercion)]) in
        (match __ret with
  C_double ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_get_value m v =
    let __ret = _yices_get_value (C_list [(match m with
  Yices_model _m -> _m
| _ -> raise NoCoercion); (match v with
  Yices_var_decl _v -> _v
| _ -> raise NoCoercion)]) in
        (match (enum_to_int `lbool __ret) with
   C_int ___ret -> ___ret
 | _ -> raise NoCoercion)

let yices_get_bitvector_value m d n bv =
    let __ret = _yices_get_bitvector_value (C_list [(match m with
  Yices_model _m -> _m
| _ -> raise NoCoercion); (match d with
  Yices_var_decl _d -> _d
| _ -> raise NoCoercion); (C_int32 n); (C_array (Array.map (fun bv -> (C_int bv)) bv))]) in
        (match __ret with
  C_int ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_mk_true ctx =
    let __ret = _yices_mk_true (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_false ctx =
    let __ret = _yices_mk_false (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bool_var ctx name =
    let __ret = _yices_mk_bool_var (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name)]) in
        (Yices_expr __ret)

let yices_mk_fresh_bool_var ctx =
    let __ret = _yices_mk_fresh_bool_var (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_get_var_decl e =
    let __ret = _yices_get_var_decl (C_list [(match e with
  Yices_expr _e -> _e
| _ -> raise NoCoercion)]) in
        (Yices_var_decl __ret)

let yices_mk_bool_var_decl ctx name =
    let __ret = _yices_mk_bool_var_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name)]) in
        (Yices_var_decl __ret)

let yices_get_var_decl_name d =
    let __ret = _yices_get_var_decl_name (C_list [(match d with
  Yices_var_decl _d -> _d
| _ -> raise NoCoercion)]) in
        (match __ret with
  C_string ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_mk_bool_var_from_decl ctx d =
    let __ret = _yices_mk_bool_var_from_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match d with
  Yices_var_decl _d -> _d
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_or ctx args =
    let __ret = _yices_mk_or (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_and ctx args =
    let __ret = _yices_mk_and (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_eq ctx a1 a2 =
    let __ret = _yices_mk_eq (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_diseq ctx a1 a2 =
    let __ret = _yices_mk_diseq (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_ite ctx c t e =
    let __ret = _yices_mk_ite (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match c with
  Yices_expr _c -> _c
| _ -> raise NoCoercion); (match t with
  Yices_expr _t -> _t
| _ -> raise NoCoercion); (match e with
  Yices_expr _e -> _e
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_not ctx a =
    let __ret = _yices_mk_not (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_type ctx name =
    let __ret = _yices_mk_type (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name)]) in
        (Yices_type __ret)

let yices_mk_function_type ctx domain range =
    let __ret = _yices_mk_function_type (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun domain -> (match domain with
  Yices_type _domain -> _domain
| _ -> raise NoCoercion)) domain)); (match range with
  Yices_type _range -> _range
| _ -> raise NoCoercion)]) in
        (Yices_type __ret)

let yices_mk_bitvector_type ctx size =
    let __ret = _yices_mk_bitvector_type (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_int32 size)]) in
        (Yices_type __ret)

let yices_mk_var_decl ctx name ty =
    let __ret = _yices_mk_var_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name); (match ty with
  Yices_type _ty -> _ty
| _ -> raise NoCoercion)]) in
        (Yices_var_decl __ret)

let yices_get_var_decl_from_name ctx name =
    let __ret = _yices_get_var_decl_from_name (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name)]) in
        (Yices_var_decl __ret)

let yices_mk_var_from_decl ctx d =
    let __ret = _yices_mk_var_from_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match d with
  Yices_var_decl _d -> _d
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_create_var_decl_iterator c =
    let __ret = _yices_create_var_decl_iterator (C_list [(match c with
  Yices_context _c -> _c
| _ -> raise NoCoercion)]) in
        (Yices_var_decl_iterator __ret)

let yices_iterator_has_next it =
    let __ret = _yices_iterator_has_next (C_list [(match it with
  Yices_var_decl_iterator _it -> _it
| _ -> raise NoCoercion)]) in
        (match __ret with
  C_int ___ret -> ___ret
| _ -> raise NoCoercion)

let yices_iterator_next it =
    let __ret = _yices_iterator_next (C_list [(match it with
  Yices_var_decl_iterator _it -> _it
| _ -> raise NoCoercion)]) in
        (Yices_var_decl __ret)

let yices_iterator_reset it =
    let __ret = _yices_iterator_reset (C_list [(match it with
  Yices_var_decl_iterator _it -> _it
| _ -> raise NoCoercion)]) in
        ()

let yices_mk_var_decl ctx name ty =
    let __ret = _yices_mk_var_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string name); (match ty with
  Yices_type _ty -> _ty
| _ -> raise NoCoercion)]) in
        (Yices_var_decl __ret)

let yices_del_iterator it =
    let __ret = _yices_del_iterator (C_list [(match it with
  Yices_var_decl_iterator _it -> _it
| _ -> raise NoCoercion)]) in
        ()

let yices_mk_var_from_decl ctx d =
    let __ret = _yices_mk_var_from_decl (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match d with
  Yices_var_decl _d -> _d
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_app ctx f args =
    let __ret = _yices_mk_app (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match f with
  Yices_expr _f -> _f
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_num ctx n =
    let __ret = _yices_mk_num (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_int n)]) in
        (Yices_expr __ret)

let yices_mk_num_from_string ctx n =
    let __ret = _yices_mk_num_from_string (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_string n)]) in
        (Yices_expr __ret)

let yices_mk_sum ctx args =
    let __ret = _yices_mk_sum (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_sub ctx args =
    let __ret = _yices_mk_sub (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_mul ctx args =
    let __ret = _yices_mk_mul (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_array (Array.map (fun args -> (match args with
  Yices_expr _args -> _args
| _ -> raise NoCoercion)) args))]) in
        (Yices_expr __ret)

let yices_mk_lt ctx a1 a2 =
    let __ret = _yices_mk_lt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_le ctx a1 a2 =
    let __ret = _yices_mk_le (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_gt ctx a1 a2 =
    let __ret = _yices_mk_gt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_ge ctx a1 a2 =
    let __ret = _yices_mk_ge (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_constant_from_array ctx size bv =
    let __ret = _yices_mk_bv_constant_from_array (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_int32 size); (C_array (Array.map (fun bv -> (C_int bv)) bv))]) in
        (Yices_expr __ret)

let yices_mk_bv_add ctx a1 a2 =
    let __ret = _yices_mk_bv_add (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_sub ctx a1 a2 =
    let __ret = _yices_mk_bv_sub (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_mul ctx a1 a2 =
    let __ret = _yices_mk_bv_mul (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_minus ctx a1 =
    let __ret = _yices_mk_bv_minus (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_concat ctx a1 a2 =
    let __ret = _yices_mk_bv_concat (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_and ctx a1 a2 =
    let __ret = _yices_mk_bv_and (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_or ctx a1 a2 =
    let __ret = _yices_mk_bv_or (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_xor ctx a1 a2 =
    let __ret = _yices_mk_bv_xor (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_not ctx a1 =
    let __ret = _yices_mk_bv_not (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_extract ctx e b a =
    let __ret = _yices_mk_bv_extract (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (C_int32 e); (C_int32 b); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_sign_extend ctx a n =
    let __ret = _yices_mk_bv_sign_extend (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion); (C_int32 n)]) in
        (Yices_expr __ret)

let yices_mk_bv_shift_left0 ctx a n =
    let __ret = _yices_mk_bv_shift_left0 (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion); (C_int32 n)]) in
        (Yices_expr __ret)

let yices_mk_bv_shift_left1 ctx a n =
    let __ret = _yices_mk_bv_shift_left1 (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion); (C_int32 n)]) in
        (Yices_expr __ret)

let yices_mk_bv_shift_right0 ctx a n =
    let __ret = _yices_mk_bv_shift_right0 (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion); (C_int32 n)]) in
        (Yices_expr __ret)

let yices_mk_bv_shift_right1 ctx a n =
    let __ret = _yices_mk_bv_shift_right1 (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a with
  Yices_expr _a -> _a
| _ -> raise NoCoercion); (C_int32 n)]) in
        (Yices_expr __ret)

let yices_mk_bv_lt ctx a1 a2 =
    let __ret = _yices_mk_bv_lt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_le ctx a1 a2 =
    let __ret = _yices_mk_bv_le (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_gt ctx a1 a2 =
    let __ret = _yices_mk_bv_gt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_ge ctx a1 a2 =
    let __ret = _yices_mk_bv_ge (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_slt ctx a1 a2 =
    let __ret = _yices_mk_bv_slt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_sle ctx a1 a2 =
    let __ret = _yices_mk_bv_sle (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_mk_bv_sgt ctx a1 a2 =
    let __ret = _yices_mk_bv_sgt (C_list [(match ctx with
  Yices_context _ctx -> _ctx
| _ -> raise NoCoercion); (match a1 with
  Yices_expr _a1 -> _a1
| _ -> raise NoCoercion); (match a2 with
  Yices_expr _a2 -> _a2
| _ -> raise NoCoercion)]) in
        (Yices_expr __ret)

let yices_pp_expr e =
    let __ret = _yices_pp_expr (C_list [(match e with
  Yices_expr _e -> _e
| _ -> raise NoCoercion)]) in
        ()
				
				
let yices_get_int_value m v = 
	let l = Array.make 1 (Swig.make_int64 0) in
	let __ret = Oyicesunsafe._yices_get_int_value (Swig.C_list [
		(match m with
  		Yices_model _m -> _m
			| _ -> raise NoCoercion); 
		(match v with
  		Yices_var_decl _v -> _v
			| _ -> raise NoCoercion);
		(Array.get l 0)
			]) in
	let (value, result) = match __ret with
		| Swig.C_list l -> (List.nth l 0, List.nth l 1)
		| _ -> raise NoCoercion in
	let result = Oyicesunsafe.enum_to_int `lbool result in
  (match result with
		Swig.C_int value' -> 
			if (value' = 1) then 
				match value with 
					| Swig.C_int64 vint -> 
						Int64.to_int vint
					| _ -> raise NoCoercion
			else (Printf.printf "library error code = %d\n" value'; raise NoCoercion)
 		| _ -> (Printf.printf "%s\n" "library error"; raise NoCoercion))