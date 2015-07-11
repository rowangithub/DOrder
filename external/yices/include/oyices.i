%module oyicesunsafe
%{
  #include "yices_c.h"
%}

%typemap(in) (yices_expr args[], unsigned n) {
    int i;
    CAML_VALUE _arr;

    /* $*1_type */
    _arr = $input;
    $2 = caml_array_len(_arr);
    $1 = ($*1_type *)malloc($2 * sizeof( yices_expr ));
    for( i = 0; i < $2; i++ ) {
        $1[i] = *((yices_expr *) caml_ptr_val(caml_array_nth(_arr,i),SWIGTYPE_p_yices_expr));
    }
}

%typemap(in) (yices_type domain[], unsigned domain_size) {
    int i;
    CAML_VALUE _arr;

    /* $*1_type */
    _arr = $input;
    $2 = caml_array_len(_arr);
    $1 = ($*1_type *)malloc($2 * sizeof( yices_type ));
    for( i = 0; i < $2; i++ ) {
        $1[i] = *((yices_type *) caml_ptr_val(caml_array_nth(_arr,i),SWIGTYPE_p_yices_type));
    }
}

enum lbool { l_false=-1, l_undef, l_true };

extern void yices_set_verbosity(int l);
extern char * yices_version();
extern void yices_set_max_num_conflicts_in_maxsat_iteration(unsigned n);
extern void yices_enable_type_checker(int flag);
extern void yices_set_max_num_iterations_in_maxsat (unsigned n);
extern void yices_set_maxsat_initial_cost(long long c);
extern void yices_set_arith_only(int flag);
extern void yices_enable_log_file(char * file_name);

extern yices_context yices_mk_context();
extern void yices_del_context(yices_context ctx);
extern void yices_reset(yices_context ctx);
extern void yices_dump_context(yices_context ctx);

extern void yices_push(yices_context ctx);
extern void yices_pop(yices_context ctx);

extern void yices_assert(yices_context ctx, yices_expr expr);
extern assertion_id yices_assert_weighted(yices_context ctx, yices_expr expr, long long w);
extern assertion_id yices_assert_retractable(yices_context ctx, yices_expr expr);
extern void yices_retract(yices_context ctx, assertion_id id);
extern int yices_inconsistent(yices_context ctx);
extern lbool yices_check(yices_context ctx);

extern lbool yices_find_weighted_model(yices_context ctx, int random);
extern lbool yices_evaluate_in_model(yices_model m, yices_expr e);
extern lbool yices_max_sat(yices_context ctx);
extern lbool yices_max_sat_cost_leq(yices_context c, long long max_cost);
extern yices_model yices_get_model(yices_context ctx);
extern void yices_display_model(yices_model m);
extern long long yices_get_cost(yices_model m);
extern double yices_get_cost_as_double(yices_model m);

extern lbool yices_get_value(yices_model m, yices_var_decl v);
%include typemaps.i
extern int yices_get_int_value(yices_model INPUT, yices_var_decl INPUT, long * OUTPUT);
extern int yices_get_arith_value(yices_model m, yices_var_decl d, long * num, long * den);
extern int yices_get_double_value(yices_model m, yices_var_decl d, double * value);
extern int yices_get_bitvector_value(yices_model m, yices_var_decl d, unsigned n, int bv[]);
extern int yices_get_assertion_value(yices_model m, assertion_id id);

extern yices_expr yices_mk_true(yices_context ctx);
extern yices_expr yices_mk_false(yices_context ctx);

extern yices_expr yices_mk_bool_var(yices_context ctx, char * name);
extern yices_expr yices_mk_fresh_bool_var(yices_context ctx);
extern yices_var_decl yices_get_var_decl(yices_expr e);
extern yices_var_decl yices_mk_bool_var_decl(yices_context ctx, char * name);
extern char * yices_get_var_decl_name(yices_var_decl d);
extern yices_expr yices_mk_bool_var_from_decl(yices_context ctx, yices_var_decl d);

extern yices_expr yices_mk_or(yices_context ctx, yices_expr args[], unsigned n);
extern yices_expr yices_mk_and(yices_context ctx, yices_expr args[], unsigned n);

extern yices_expr yices_mk_eq(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_diseq(yices_context ctx, yices_expr a1, yices_expr a2);

extern yices_expr yices_mk_ite(yices_context ctx, yices_expr c, yices_expr t, yices_expr e);

extern yices_expr yices_mk_not(yices_context ctx, yices_expr a);

extern yices_type yices_mk_type(yices_context ctx, char * name);
extern yices_type yices_mk_function_type(yices_context ctx, yices_type domain[], unsigned domain_size, yices_type range);

extern yices_type yices_mk_bitvector_type(yices_context ctx, unsigned size);
extern yices_type yices_mk_tuple_type(yices_context ctx, yices_type * args[], unsigned size);

extern yices_var_decl yices_mk_var_decl(yices_context ctx, char * name, yices_type ty);
extern yices_var_decl yices_get_var_decl_from_name(yices_context ctx, char * name);
extern yices_expr yices_mk_var_from_decl(yices_context ctx, yices_var_decl d);

extern yices_var_decl_iterator yices_create_var_decl_iterator(yices_context c);
extern int yices_iterator_has_next(yices_var_decl_iterator it);
extern yices_var_decl yices_iterator_next(yices_var_decl_iterator it);
extern void yices_iterator_reset(yices_var_decl_iterator it);
extern yices_var_decl yices_mk_var_decl(yices_context ctx, char * name, yices_type ty);
extern void yices_del_iterator(yices_var_decl_iterator it);
extern yices_expr yices_mk_var_from_decl(yices_context ctx, yices_var_decl d);

extern yices_expr yices_mk_app(yices_context ctx, yices_expr f, yices_expr args[], unsigned n);

extern yices_expr yices_mk_num(yices_context ctx, int n);
extern yices_expr yices_mk_num_from_string(yices_context ctx, char * n);

extern yices_expr yices_mk_sum(yices_context ctx, yices_expr args[], unsigned n);
extern yices_expr yices_mk_sub(yices_context ctx, yices_expr args[], unsigned n);
extern yices_expr yices_mk_mul(yices_context ctx, yices_expr args[], unsigned n);
extern yices_expr yices_mk_lt(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_le(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_gt(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_ge(yices_context ctx, yices_expr a1, yices_expr a2);

extern yices_expr yices_mk_bv_constant(yices_context ctx, unsigned size, unsigned long value);
extern yices_expr yices_mk_bv_constant_from_array(yices_context ctx, unsigned size, int bv[]);
extern yices_expr yices_mk_bv_add(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_sub(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_mul(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_minus(yices_context ctx, yices_expr a1);
extern yices_expr yices_mk_bv_concat(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_and(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_or(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_xor(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_not(yices_context ctx, yices_expr a1);
extern yices_expr yices_mk_bv_extract(yices_context ctx, unsigned e, unsigned b, yices_expr a);
extern yices_expr yices_mk_bv_sign_extend(yices_context ctx, yices_expr a, unsigned n);
extern yices_expr yices_mk_bv_shift_left0(yices_context ctx, yices_expr a, unsigned n);
extern yices_expr yices_mk_bv_shift_left1(yices_context ctx, yices_expr a, unsigned n);
extern yices_expr yices_mk_bv_shift_right0(yices_context ctx, yices_expr a, unsigned n);
extern yices_expr yices_mk_bv_shift_right1(yices_context ctx, yices_expr a, unsigned n);
extern yices_expr yices_mk_bv_lt(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_le(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_gt(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_ge(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_slt(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_sle(yices_context ctx, yices_expr a1, yices_expr a2);
extern yices_expr yices_mk_bv_sgt(yices_context ctx, yices_expr a1, yices_expr a2);

extern void yices_pp_expr(yices_expr e);
