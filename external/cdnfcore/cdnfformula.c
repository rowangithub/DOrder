/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
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



/*
 * vector of length 0 is the unit monomial (T)
 */
inline static monomial *cdnfformula_monomial_unit (void)
{
  return vector_new (0);
}

inline static monomial *cdnfformula_monomial_add (monomial *m, lit l)
{
  assert (l != 0);
  vector_add (m, (pointer_t)l);
  return m;
}

/*
 * bv : an assignment to variables (indexed from 1)
 * I == { i : bv[i]}
 * M_DNF (bv) == /\_{i in I} X_i if I != empty
              == T               if I == empty
 */
inline monomial *cdnfformula_monomial_M_DNF (bitvector *bv)
{
  uscalar_t i;
  monomial *result;
  result = cdnfformula_monomial_unit ();
  for (i = bitvector_length (bv) - 1; i > 0; i--) {
    if (bitvector_get (bv, i))
      result = cdnfformula_monomial_add (result, i);
  }
  assert (vector_length (result) > 0);
  return result;
}

/*
 * vector of length 0 is the unit of disjunction (F)
 */
inline disjunction *cdnfformula_disjunction_unit (void)
{
  return vector_new (0);
}

inline disjunction *cdnfformula_disjunction_new (uscalar_t length)
{
  return vector_new (length);
}

inline disjunction *cdnfformula_disjunction_add (disjunction *f,
                                                 monomial *disj)
{
  vector_add (f, disj);
  return f;
}

inline void cdnfformula_disjunction_free (disjunction *disj)
{
  uscalar_t i, num_disjs;

  num_disjs = vector_length (disj);
  for (i = 0; i < num_disjs; i++) {
    monomial *mono;

    mono = (monomial *) vector_get (disj, i);
    vector_free (mono);
  }
  vector_free (disj);
}

/*
 * vector of length 0 is the unit of conjunction (T)
 */
inline conjunction *cdnfformula_conjunction_unit (void)
{
  return vector_new (0);
}

inline conjunction *cdnfformula_conjunction_new (uscalar_t length)
{
  return vector_new (length);
}

inline conjunction *cdnfformula_conjunction_add (conjunction *f,
                                                 disjunction *conj)
{
  vector_add (f, conj);
  return f;
}

inline void cdnfformula_free (conjunction *f)
{
  uscalar_t i, num_conjs;

  num_conjs = vector_length (f);
  for (i = 0; i < num_conjs; i++) {
    disjunction *disj;

    disj = (disjunction *) vector_get (f, i);
    cdnfformula_disjunction_free (disj);
  }
  vector_free (f);
}

static void cdnfformula_monomial_to_string (monomial *m)
{
  if (vector_length (m) == 0) {
    fprintf (stderr, "( T )");
  } else {
    uscalar_t i;
    fprintf (stderr, "( ");
    for (i = vector_length (m) - 1; i > 0; i--) {
      fprintf (stderr, "%ld & ", (lit) vector_get (m, i));
    }
    assert (i == 0);
    fprintf (stderr, "%ld ", (lit) vector_get (m, i));
    fprintf (stderr, " )");
  }
}

static void cdnfformula_disjunction_to_string (disjunction *f)
{
  if (vector_length (f) == 0) {
    fprintf (stderr, "[ F ]");
  } else {
    uscalar_t i;
    fprintf (stderr, "[ ");
    for (i = vector_length (f) - 1; i > 0; i--) {
      cdnfformula_monomial_to_string (vector_get (f, i));
      fprintf (stderr, " | ");
    }
    assert (i == 0);
    cdnfformula_monomial_to_string (vector_get (f, i));
    fprintf (stderr, " ]");
  }
}

void cdnfformula_print (conjunction *f)
{
  if (vector_length (f) == 0) {
    fprintf (stderr, "{ T }");
  } else {
    uscalar_t i;
    fprintf (stderr, "{ ");
    for (i = vector_length (f) - 1; i > 0; i--) {
      cdnfformula_disjunction_to_string (vector_get (f, i));
      fprintf (stderr, " & ");
    }
    assert (i == 0);
    cdnfformula_disjunction_to_string (vector_get (f, i));
    fprintf (stderr, " }\n");
  }
}

/*
 *
 */
static bool cdnfformula_eval_monomial (monomial *m, bitvector *bv)
{
  uscalar_t i, length;
  length = vector_length (m);
  for (i = 0; i < length; i++) {
    lit l;
    var v;
    l = (lit) vector_get (m, i);
    v = boolformula_var_from_lit (l);
    assert (0 < v);
    assert (v < bitvector_length (bv));
    if (boolformula_positive_lit (l) != bitvector_get (bv, v))
      return false;
  }
  return true;
}

bool cdnfformula_eval_M_DNF (disjunction *m_dnf, bitvector *bv)
{
  uscalar_t i, length;
  length = vector_length (m_dnf);
  for (i = 0; i < length; i++) {
    monomial *m;
    m = vector_get (m_dnf, i);
    if (cdnfformula_eval_monomial (m, bv))
      return true;
  }
  return false;
}

inline boolformula_t* monomial_to_boolformula (monomial *m)
{
  boolformula_t* ret=boolformula_conjunction_new(vector_length(m)),*temp;

  uscalar_t i;
  for (i = 0; i < vector_length (m); i++) {
    temp=boolformula_literal_new((lit)vector_get(m,i));
    boolformula_set(ret, i, temp);
    boolformula_free(temp);
  }
  return ret;
}

inline boolformula_t* dnf_to_boolformula (disjunction *f)
{
  boolformula_t* ret=boolformula_disjunction_new(vector_length(f)), *temp;

  uscalar_t i;
  for (i = 0; i < vector_length (f); i++) {
    temp=monomial_to_boolformula(vector_get(f,i));
    boolformula_set(ret, i, temp);
    boolformula_free(temp);
  }
  return ret;
}

inline boolformula_t *cdnfformula_to_boolformula (conjunction * f){
  boolformula_t* ret=boolformula_conjunction_new(vector_length(f)), *temp;
  uscalar_t i;
  for (i = 0; i < vector_length (f); i++) {
    temp=dnf_to_boolformula(vector_get(f,i));
    boolformula_set(ret, i, temp);
    boolformula_free(temp);
  }
  return ret;
}
