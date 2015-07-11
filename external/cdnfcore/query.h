/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#ifndef _LIBCDNFP_QUERY_H_
#define _LIBCDNFP_QUERY_H_

typedef bool membership_result_t;
typedef struct {
  bool is_equal;
  bitvector *counterexample;
} equivalence_result_t;

typedef membership_result_t (*membership_t) (void *info, bitvector *bv);
typedef equivalence_result_t *(*equivalence_t) (void *info, uscalar_t num_vars, boolformula_t* b);

#endif
