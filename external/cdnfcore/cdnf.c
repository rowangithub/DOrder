/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Change log: 2014/2/18 Add support of CNDF_Plus3 --Yu-Fang Chen
 * Copyright reserved
 *****************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "type.h"
#include "bitvector.h"
#include "vector.h"
#include "boolformula.h"
#include "cdnfformula.h"
#include "query.h"
#include "cdnf.h"


typedef struct {
  disjunction *H;
  vector *T;
  bitvector *a;
} basis;

//#define DEBUG

/*
 * walk from v toward a while keeping f (v) == 1
 */
void walk (void *info, 
           membership_t membership, bitvector *v, bitvector *a)
{
  uscalar_t i;

  assert (bitvector_length (v) == bitvector_length (a));
  i = bitvector_length (a) - 1;
  while (i > 0) {
    if (bitvector_get (v, i) != bitvector_get (a, i)) {
      bool b = bitvector_get (v, i);
      bitvector_set (v, i, !b);
      if ((*membership) (info, v)) {
        i = bitvector_length (a);
      } else {
        bitvector_set (v, i, b);
      }
    }
    i--;
  }
}

static void add_basis (vector *bases, bitvector *a) 
{
  basis *b;

  b = (basis *) malloc (sizeof (basis));
  assert (b != NULL);
  b->H = cdnfformula_disjunction_unit ();
  b->T = vector_new (0);
  b->a = bitvector_copy (a);
  
  vector_add (bases, b);
}

static conjunction *get_conjecture (vector *bases)
{
  conjunction *conjecture;
  uscalar_t i, length;

  length = vector_length (bases);
  conjecture = cdnfformula_conjunction_new (length);
  for (i = 0; i < length; i++) {
    disjunction *H;

    H = ((basis *)vector_get (bases, i))->H;
    vector_set (conjecture, i, H);
  }
  return conjecture;
}

vector *get_indices_to_modify (vector *bases, bitvector *v)
{
  vector *result;
  uscalar_t i, length;
  
  result = vector_new (0);
  length = vector_length (bases);
  for (i = 0; i < length; i++) {
    basis *b;
    b = (basis *) vector_get (bases, i);
    if (!cdnfformula_eval_M_DNF (b->H, v))
      vector_add (result, (pointer_t)i);
  }
  return result;
}

monomial *compute_m_of_x_xor_a (monomial *m, bitvector *a)
{
  uscalar_t i, length;
  length = vector_length (m);
  for (i = 0; i < length; i++) {
    lit l;
    l = (lit) vector_get (m, i);
    assert (boolformula_var_from_lit (l) < bitvector_length (a));
    if (bitvector_get (a, boolformula_var_from_lit (l))) {
      l = boolformula_lit_complement (l);
    }
    vector_set (m, i, (pointer_t) l);
  }
  return m;
}

void rebuild_disjunctions (vector *bases)
{
  uscalar_t i, length;

  length = vector_length (bases);

  for (i = 0; i < length; i++) {
    basis *B;
    bitvector *a_i;
    vector *T_i;
    disjunction *H_i;
    uscalar_t j, T_i_len;
    monomial *m;
    
    B = (basis *) vector_get (bases, i);
    a_i = B->a;
    T_i = B->T;
    assert (B->H == NULL);
    H_i = cdnfformula_disjunction_unit ();
    T_i_len = vector_length (T_i);

    for (j = 0; j < T_i_len; j++) {
      bitvector *xor, *v;

      v = (bitvector *) vector_get (T_i, j);
      xor = bitvector_xor (v, a_i);
      /* when xor == 0, a conflict has occurred. */
      assert (!bitvector_is_zeros (xor));

      /* compute M_DNF (v_i + a_i) */
      m = cdnfformula_monomial_M_DNF (xor);
      /* compute m (x ^ a_i) */
      m = compute_m_of_x_xor_a (m, a_i);
      vector_add (H_i, m);
      bitvector_free (xor);
    }
    B->H = H_i;
  }
}

void refine_bases (void *info, 
                   membership_t membership, membership_t comembership,
                   vector *bases, int mode)
{
  uscalar_t i, length;

  length = vector_length (bases);
#ifdef DEBUG    
  fprintf (stderr, "refine bases, num of DNF = %d",length);
#endif
  for (i = 0; i < length; i++) {
    basis *B;
    bitvector *a_i;
    vector *T_i;
    uscalar_t l, T_i_len, j;
    bool b;

    B = (basis *) vector_get (bases, i);
    /* 
     * extends each unsatisfying assignment by FALSE
     */
    a_i = B->a;
    l = bitvector_length (a_i);
    bitvector_resize (a_i, l + 1);
    bitvector_set (a_i, l, false);
    if(mode==CDNF_PlusPlus)
    	b = (*comembership) (info, a_i) == true ? false : true;
    else
	b=false;
    bitvector_set (a_i, l, b);
#ifdef DEBUG
    fprintf (stderr, "extends basis: ");
    bitvector_print (a_i);
#endif

#ifdef DEBUG
      fprintf (stderr, "extends support: ");
#endif
    T_i = B->T;
    T_i_len = vector_length (T_i);
    /* except the last basis, all bases have at least one support */
    assert (i == length - 1 || T_i_len > 0);
    for (j = 0; j < T_i_len; j++) {
      bitvector *v;
      bool c;

      /*
       * extends v to v+b if MEM (v+b) = YES
       *           to v+!b if MEM (v+b) = NO
       */
      v = (bitvector *)vector_get (T_i, j);
      assert (bitvector_length (v) == l);
      bitvector_resize (v, l + 1);
      bitvector_set (v, l, b);
      c = (*membership) (info, v) == true ? b : !b;
      if (c != b)
        bitvector_set (v, l, c);
#ifdef DEBUG
      bitvector_print (v);
#endif
    }
    /* clean up disjunction for the current basis */
    cdnfformula_disjunction_free (B->H);
    B->H = NULL;

    /* remove the last basis if it has no support */
    if (T_i_len == 0) {
      assert (i == length - 1);
      bitvector_free (a_i);
      vector_free (T_i);
      free (vector_get (bases, length - 1));
      vector_resize (bases, length - 1);
    }
  }
  /* 
   * reconstruct disjunctions for the bases
   */
  rebuild_disjunctions (bases);
}

void cleanup (vector *bases)
{
  uscalar_t i, length;
  length = vector_length (bases);
  for (i = 0; i < length; i++) {
    basis *B;
    vector *T;
    uscalar_t j, len;

    B = vector_get (bases, i);
    cdnfformula_disjunction_free (B->H);
    bitvector_free (B->a);
    T = B->T;
    len = vector_length (T);
    for (j = 0; j < len; j++) {
      bitvector_free (vector_get (T, j));
    }
    vector_free (T);
    free (B);
  }
  vector_free (bases);
}


boolformula_t *learn_core (void *info, 
                      uscalar_t num_vars, 
                      membership_t membership, membership_t comembership,
                      equivalence_t equivalence, int mode)
{
  equivalence_result_t *eq_result;
  conjunction *conjecture;
  vector *bases;

  bases = vector_new (0);
  conjecture = get_conjecture (bases);
  boolformula_t* b_conjecture = cdnfformula_to_boolformula (conjecture);
  vector_free (conjecture);
  eq_result = (*equivalence) (info, num_vars, boolformula_copy (b_conjecture));
  if (eq_result->is_equal){
    //fprintf(stderr,"Number of Variables Used : %d\n", (unsigned int)num_vars);
    free(eq_result);
    return b_conjecture;
  } else {
    boolformula_free(b_conjecture);
  }

#ifdef DEBUG
  fprintf (stderr, "add first basis with ");
  bitvector_print (eq_result->counterexample);
#endif

  assert (bitvector_length (eq_result->counterexample) == num_vars + 1);
  add_basis (bases, eq_result->counterexample);
  bitvector_free (eq_result->counterexample);
  free(eq_result);
  while (true) {
    vector *I;
    bitvector *v;
    uscalar_t j, length;

    conjecture = get_conjecture (bases);
#ifdef DEBUG
    fprintf (stderr, "new conjecture = ");
    boolformula_print (conjecture);
#endif
    boolformula_t* b_conjecture = cdnfformula_to_boolformula (conjecture);
    vector_free (conjecture);
    eq_result = (*equivalence) (info, num_vars, boolformula_copy (b_conjecture));

    if (eq_result->is_equal) {
      //fprintf(stderr,"Number of Variables Used : %d\n", (unsigned int)num_vars);
      cleanup (bases);
      free (eq_result);
      return b_conjecture;
    } else {
      boolformula_free(b_conjecture);
    }

    /* H_i's are still in bases, only free the conjunction */
    assert (bitvector_length (eq_result->counterexample) == num_vars + 1);
    v = eq_result->counterexample;
    I = get_indices_to_modify (bases, v);
    if (vector_length (I) == 0) {
      if(mode==CDNF_Plus3 && (*membership)(info,v)==true){
#ifdef DEBUG      
        fprintf (stderr, "conflict detected on: ");
        bitvector_print (v);
        fprintf (stderr, "num of variables: %d \n",num_vars);
#endif
	refine_bases(info, membership,comembership,bases,mode);
	num_vars++;
      }else{
#ifdef DEBUG      
        fprintf (stderr, "add basis: ");
        bitvector_print (v);
#endif
        add_basis (bases, v);
      } 
      bitvector_free (v);
      free(eq_result);
      vector_free (I);
      continue;
    }
    free(eq_result);

#ifdef DEBUG    
    fprintf (stderr, "fix m_dnf with ");
    bitvector_print (v);
#endif
    length = vector_length (I);
    for (j = 0; j < length; j++) {
      uscalar_t i;
      basis *B;
      bitvector *a_i, *v_i, *xor;
      vector *T_i;
      disjunction *H_i;
      monomial *m;

      i = (uscalar_t) vector_get (I, j);
      B = (basis *) vector_get (bases, i);
      a_i = B->a;
      T_i = B->T;
      H_i = B->H;

      v_i = bitvector_copy (v);
      walk (info, membership, v_i, a_i);
      xor = bitvector_xor (v_i, a_i);
      /* when xor == 0, a conflict has occurred. */
      /* assert (!bitvector_is_zeros (xor)); */
      if (bitvector_is_zeros (xor)) {
#ifdef DEBUG
        fprintf (stderr, "conflict with the basis ");
        bitvector_print (a_i);
#endif
        bitvector_free (xor);
        bitvector_free (v_i);
        if(mode==CDNF_PlusPlus||mode==CDNF_Plus3){
          /* increase the number of variables */
#ifdef DEBUG
        fprintf (stderr, "num of variables: %d \n",num_vars);
#endif
          refine_bases (info, membership, comembership, bases,mode);
          num_vars++;
        }else{
          bitvector_free (v);
          vector_free (I);
          return NULL;
        }
        break;
      }
#ifdef DEBUG
      fprintf (stderr, "add support ");
      bitvector_print (v_i);
#endif
      /* store v_i in T_i */
      /* note that the original CDNF algorithm stores xor in S_i */
      vector_add (T_i, v_i);

      /* compute M_DNF (v_i + a_i) */
      m = cdnfformula_monomial_M_DNF (xor);
      /* compute m (x ^ a_i) */
      m = compute_m_of_x_xor_a (m, a_i);
      vector_add (H_i, m);
      bitvector_free (xor);
    }
    bitvector_free (v);
    vector_free (I);
  }
}


boolformula_t *learn (void *info, 
                      uscalar_t num_vars, 
                      membership_t membership, membership_t comembership,
                      equivalence_t equivalence, int mode)
{
  if(mode==CDNF_Plus){
	boolformula_t* result=learn_core(info,num_vars,membership,comembership, equivalence, mode);
	while(result==NULL){
		num_vars++;
		result=learn_core(info,num_vars,membership,comembership, equivalence, mode);
	}
	return result;
  }else{
	return learn_core(info,num_vars,membership,comembership, equivalence, mode);
  }
}


