/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include "type.h"
#include "vector.h"
#include "bitvector.h"
#include "boolformula.h"
#include "query.h"
#include "cdnf.h"

/* bitvector */

static value boolarray_from_bitvector (bitvector *bv)
{
  CAMLparam0 ();
  CAMLlocal1 (result);
  uscalar_t i, length;

  length = bitvector_length (bv);
  result = caml_alloc_tuple (length);
  Store_field (result, 0, Val_false);

  for (i = length - 1; i > 0; i--) {
    Store_field (result, i, bitvector_get (bv, i) ? Val_true : Val_false);
  }
  CAMLreturn (result);
}

static bitvector *bitvector_from_boolarray (value bary)
{
  CAMLparam1 (bary);
  bitvector *bv;
  uscalar_t i, length;

  length = Wosize_val (bary);
  bv = bitvector_new (length);
  bitvector_set (bv, 0, false);
  for (i = 1; i < length; i++) {
    bitvector_set (bv, i, Field (bary, i) == Val_true ? true : false);
  }
  
  CAMLreturnT (bitvector *, bv);
}

/* boolformula */

static void dealloc_boolformula (value bf);
enum { CDNFP_LIT = 0, CDNFP_AND = 1, CDNFP_OR = 2 };

static struct custom_operations boolformula_ops = {
  "cdnfp.boolformula",
  // custom_finalize_default,
  dealloc_boolformula,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define boolformula_val(v) (*((boolformula_t **) Data_custom_val(v)))

static inline value Val_boolformula (boolformula_t *bool_f)
{
  CAMLparam0 ();
  CAMLlocal1 (result);

  result = alloc_custom (&boolformula_ops, sizeof (boolformula_t *), 0, 1);
  boolformula_val (result) = bool_f;
  bool_f->ref++;
  CAMLreturn (result);
}

static void dealloc_boolformula (value bf) 
{
  boolformula_t *bool_f = boolformula_val (bf);
  boolformula_free (bool_f);
}

value ocaml_boolformula_lit_from_var (value v)
{
  CAMLparam1 (v);
  CAMLreturn (Val_int (boolformula_lit_from_var (Int_val (v))));
}

value ocaml_boolformula_var_from_lit (value l)
{
  CAMLparam1 (l);
  CAMLreturn (Val_int (boolformula_var_from_lit (Int_val (l))));
}

value ocaml_boolformula_lit_complement (value l)
{
  CAMLparam1 (l);
  CAMLreturn (Val_int (boolformula_lit_complement (Int_val (l))));
}

value ocaml_boolformula_positive_lit (value l)
{
  CAMLparam1 (l);
  CAMLreturn (boolformula_positive_lit (Int_val (l)) == false ?
              Val_false : Val_true);
}

value ocaml_create_literal (value l) 
{
  CAMLparam1 (l);
  CAMLlocal1 (result);
  boolformula_t *bool_f;

  bool_f = boolformula_literal_new (Int_val (l));
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_create_disjunction (value l)
{
  CAMLparam1 (l);
  CAMLlocal1 (result);
  uscalar_t len;
  boolformula_t *bool_f;

  len = Int_val (l);
  bool_f = len > 0 ? 
    boolformula_disjunction_new (len) : boolformula_disjunction_unit ();
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_create_conjunction (value l)
{
  CAMLparam1 (l);
  CAMLlocal1 (result);
  uscalar_t len;
  boolformula_t *bool_f;

  len = Int_val (l);
  bool_f = len > 0 ? 
    boolformula_conjunction_new (len) : boolformula_conjunction_unit ();
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_add_boolformula (value p, value c)
{
  CAMLparam2 (p, c);
  CAMLlocal1 (result);
  boolformula_t *parent, *child, *bool_f;

  parent = boolformula_val (p);
  child = boolformula_val (c);
  bool_f = boolformula_add (parent, child);
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_set_boolformula (value p, value i, value c)
{
  CAMLparam3 (p, i, c);
  CAMLlocal1 (result);
  boolformula_t *parent, *child, *bool_f;

  parent = boolformula_val (p);
  child = boolformula_val (c);
  bool_f = boolformula_set (parent, Int_val (i), child);
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_get_type (value f)
{
  CAMLparam1 (f);
  CAMLlocal1 (result);
  boolformula_t *bool_f;

  bool_f = boolformula_val (f);
  switch (boolformula_get_type (bool_f)) {
  case disjunct: result = Val_int (CDNFP_OR); break;
  case conjunct: result = Val_int (CDNFP_AND); break;
  case literal: result = Val_int (CDNFP_LIT); break;
  default: assert (0);
  };
  CAMLreturn (result);
}

value ocaml_get_length (value f)
{
  CAMLparam1 (f);
  CAMLlocal1 (result);
  boolformula_t *bool_f;

  bool_f = boolformula_val (f);
  assert (boolformula_get_type (bool_f) != literal);
  result = Val_int (boolformula_get_length (bool_f));
  CAMLreturn (result);
}

value ocaml_get_boolformula (value f, value i)
{
  CAMLparam2 (f, i);
  CAMLlocal1 (result);
  boolformula_t *bool_f;
  int idx, size;

  bool_f = boolformula_val (f);
  assert (boolformula_get_type (bool_f) != literal);
  idx = Int_val (i);
  size = boolformula_get_length (bool_f);
  assert (idx < size);
  result = Val_boolformula (boolformula_get_subformula (bool_f, idx));
  CAMLreturn (result);
}

value ocaml_get_literal (value f)
{
  CAMLparam1 (f);
  CAMLlocal1 (result);
  boolformula_t *bool_f;

  bool_f = boolformula_val (f);
  assert (boolformula_get_type (bool_f) == literal);
  result = Val_int (boolformula_get_value (bool_f));
  CAMLreturn (result);
}

value ocaml_print (value f)
{
  CAMLparam1 (f);
  boolformula_t *bool_f;

  bool_f = boolformula_val (f);
  boolformula_print (bool_f);
  CAMLreturn (Val_unit);
}

value ocaml_num_vars (value f)
{
  CAMLparam1 (f);
  boolformula_t *bool_f;

  bool_f = boolformula_val (f);
  CAMLreturn (Val_int (boolformula_num_of_var (bool_f)));
}

value ocaml_to_cnf (value f, value n)
{
  CAMLparam2 (f, n);
  CAMLlocal1 (result);
  boolformula_t *bool_f;
  uscalar_t num_vars;

  bool_f = boolformula_val (f);
  num_vars = Int_val (n);
  result = Val_boolformula (boolformula_to_cnf (bool_f, num_vars));
  CAMLreturn (result);
}

value ocaml_copy (value f)
{
  CAMLparam1 (f);
  CAMLlocal1 (result);
  
  result = Val_boolformula (boolformula_copy (boolformula_val (f)));
  CAMLreturn (result);
}

/* CDNF+ */

enum { MEM_YES = Val_int (0), MEM_NO = Val_int (1) };

typedef struct {
  char *is_member_name;
  char *is_comember_name;
  char *is_equivalent_name;
} Info;

membership_result_t is_member (void *info, bitvector *bv)
{
  CAMLparam0 ();
  CAMLlocal2 (result, boolarray);
  value *is_member_closure;

  is_member_closure = 
    (value *) caml_named_value (((Info *)info)->is_member_name);
  assert (is_member_closure != NULL);
  boolarray = boolarray_from_bitvector (bv);
  result = caml_callback (*is_member_closure, boolarray);
  
  CAMLreturnT (membership_result_t, result == MEM_YES ? true : false);
}

membership_result_t is_comember (void *info, bitvector *bv)
{
  CAMLparam0 ();
  CAMLlocal2 (result, boolarray);
  value *is_comember_closure;

  is_comember_closure = 
    (value *) caml_named_value (((Info *)info)->is_comember_name);
  assert (is_comember_closure != NULL);
  boolarray = boolarray_from_bitvector (bv);
  result = caml_callback (*is_comember_closure, boolarray);
  
  CAMLreturnT (membership_result_t, result == MEM_YES ? true : false);
}

equivalence_result_t *is_equivalent (void *info,
                                     uscalar_t num_vars, boolformula_t *bool_f)
{
  CAMLparam0 ();
  CAMLlocal2 (result, f);
  equivalence_result_t *eq_result;
  value *is_equivalent_closure;
  
  is_equivalent_closure = 
    (value *) caml_named_value (((Info *)info)->is_equivalent_name);
  assert (is_equivalent_closure != NULL);
  f = Val_boolformula (bool_f);
  boolformula_free (bool_f);
  result = caml_callback2 (*is_equivalent_closure, Val_int (num_vars), f);
  eq_result = (equivalence_result_t *) malloc (sizeof (equivalence_result_t));

  /* tags of both EQ and CE bary = 0, check if result is long instead */
  eq_result->is_equal = Is_long (result);
  if (!eq_result->is_equal) {
    assert (Tag_val (result) == 0);
    assert (Tag_val (Field (result, 0)) == 0);
    eq_result->counterexample = bitvector_from_boolarray (Field (result, 0));
    /* 
     * make sure the counterexample is an assignment to [num_vars] variables
     */
    assert (bitvector_length (eq_result->counterexample) == num_vars + 1);
  } else {
    assert (result == Val_int (0));
    eq_result->counterexample = NULL;
  }

  CAMLreturnT (equivalence_result_t *, eq_result);
}


boolformula_t *ocaml_cdnf_int (value num_vars, 
                               value val_is_member, value val_is_comember,
                               value val_is_equivalent,
                               int mode)
{
  CAMLparam4 (num_vars, val_is_member, val_is_comember, val_is_equivalent);
  boolformula_t *bool_f;
  Info *info;

  info = (Info *) malloc (sizeof (Info));

  assert (Tag_val (val_is_member) == String_tag);
  info->is_member_name = 
    (char *) calloc (strlen (String_val (val_is_member))+1, sizeof (char));
  strcpy (info->is_member_name, String_val (val_is_member));

  assert (Tag_val (val_is_comember) == String_tag);
  info->is_comember_name = 
    (char *) calloc (strlen (String_val (val_is_comember))+1, sizeof (char));
  strcpy (info->is_comember_name, String_val (val_is_comember));

  assert (Tag_val (val_is_equivalent) == String_tag);
  info->is_equivalent_name = 
    (char *) calloc (strlen (String_val (val_is_equivalent))+1, sizeof (char));
  strcpy (info->is_equivalent_name, String_val (val_is_equivalent));

  bool_f = learn (info, Int_val (num_vars), is_member, is_comember, 
                  is_equivalent, mode);

  CAMLreturnT (boolformula_t *, bool_f);
}

value ocaml_cdnf (value num_vars, 
                  value val_is_member, value val_is_equivalent)
{
  CAMLparam3 (num_vars, val_is_member, val_is_equivalent);
  CAMLlocal2 (result, val_is_comember);
  boolformula_t *bool_f;

  val_is_comember = caml_alloc_string (0);
  bool_f = ocaml_cdnf_int (num_vars, 
                           val_is_member, val_is_comember, val_is_equivalent,
                           CDNF);

  if (bool_f != NULL) {
    result = caml_alloc (1, 0);
    Store_field (result, 0, Val_boolformula (bool_f));
  } else {
    result = Val_int (0);
  }

  CAMLreturn (result);
}

value ocaml_cdnfx (value num_vars, 
                   value val_is_member, value val_is_equivalent)
{
  CAMLparam3 (num_vars, val_is_member, val_is_equivalent);
  CAMLlocal2 (result, val_is_comember);
  boolformula_t *bool_f;

  val_is_comember = caml_alloc_string (0);
  bool_f = ocaml_cdnf_int (num_vars, 
                           val_is_member, val_is_comember, val_is_equivalent,
                           CDNF_Plus);

  assert (bool_f != NULL);
  result = Val_boolformula (bool_f);
  CAMLreturn (result);
}

value ocaml_cdnfxx (value num_vars, 
                    value val_is_member, value val_is_comember,
                    value val_is_equivalent)
{
  CAMLparam4 (num_vars, val_is_member, val_is_comember, val_is_equivalent);
  CAMLlocal1 (result);
  boolformula_t *bool_f;

  bool_f = ocaml_cdnf_int (num_vars, 
                           val_is_member, val_is_comember, val_is_equivalent,
                           CDNF_PlusPlus);
  /* must return a boolformula */
  assert (bool_f != NULL);

  result = Val_boolformula (bool_f);

  CAMLreturn (result);
}

value ocaml_cdnfxxx (value num_vars, 
                     value val_is_member, value val_is_equivalent)
{
  CAMLparam3 (num_vars, val_is_member, val_is_equivalent);
  CAMLlocal2 (result, val_is_comember);
  boolformula_t *bool_f;

  val_is_comember = caml_alloc_string (0);
  bool_f = ocaml_cdnf_int (num_vars, 
                           val_is_member, val_is_comember, val_is_equivalent,
                           CDNF_Plus3);
  /* must return a boolformula */
  assert (bool_f != NULL);

  result = Val_boolformula (bool_f);

  CAMLreturn (result);
}

