/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#ifndef __BITVECTOR_H_
#define __BITVECTOR_H_


#ifndef __cplusplus
#include <stdbool.h>
#endif

typedef struct {
  uscalar_t length; /* length of bit vector */
  uscalar_t size;   /* size of buffer */
  uscalar_t *buffer;         /* buffer */
} bitvector;

bitvector *bitvector_new (uscalar_t size);
void bitvector_free (bitvector *bv);
inline uscalar_t bitvector_length (bitvector *bv);
inline void bitvector_set (bitvector *bv, uscalar_t idx, bool b);
inline bool bitvector_get (bitvector *bv, uscalar_t idx);
void bitvector_resize (bitvector *bv, uscalar_t new_size);
bitvector *bitvector_xor (bitvector *bv0, bitvector *bv1);
bitvector *bitvector_copy (bitvector *bv);
bool bitvector_is_zeros (bitvector *bv);
void bitvector_print (bitvector *bv);

#endif
