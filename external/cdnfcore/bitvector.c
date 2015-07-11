/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "type.h"
#include "bitvector.h"

#define BITS_IN_INT           (8 * sizeof (unsigned int))
#define get_array_idx(idx)    ((idx) / BITS_IN_INT)
#define get_bit_pos(idx)      (1 << ((idx) % BITS_IN_INT))

static inline uscalar_t length_to_size (uscalar_t length)
{
  return 1 + length/BITS_IN_INT;
}

bitvector *bitvector_new (uscalar_t length)
{
  bitvector *result;

  result = (bitvector *) malloc (sizeof (bitvector));
  assert (result != NULL);
  result->length = length;
  result->size = length_to_size (length);
  result->buffer = length > 0 ?
    (uscalar_t *) malloc (result->size * sizeof (uscalar_t)) :
    NULL;
  assert (length == 0 || result->buffer != NULL);
  //TODO: fix me
  if(length>0)
	  bitvector_set(result,0,false);
  return result;
}

void bitvector_free (bitvector *bv)
{
  assert (bv != NULL);
  free (bv->buffer);
  free (bv);
  bv = NULL;
}

inline uscalar_t bitvector_length (bitvector *bv)
{
  return bv->length;
}

inline void bitvector_set (bitvector *bv, uscalar_t idx, bool b)
{
  assert (bv != NULL);
  assert (idx < bv->length);
  if (b)
    bv->buffer[get_array_idx(idx)] |= get_bit_pos(idx);
  else
    bv->buffer[get_array_idx(idx)] &= ~get_bit_pos(idx);
}

inline bool bitvector_get (bitvector *bv, uscalar_t idx)
{
  assert (bv != NULL);
  assert (idx < bv->length);
  return (bv->buffer[get_array_idx(idx)] & get_bit_pos(idx)) != 0;
}

void bitvector_resize (bitvector *bv, uscalar_t new_length)
{
  uscalar_t new_size;

  new_size = length_to_size (new_length);
  if (new_size > bv->size)
    bv->buffer = 
      (uscalar_t *) realloc (bv->buffer, new_size * sizeof (uscalar_t));
  bv->length = new_length;
}

bitvector *bitvector_xor (bitvector *bv0, bitvector *bv1)
{
  bitvector *result_bv;
  int i;
  assert (bv0 != NULL);
  assert (bv1 != NULL);
  assert (bv0->length == bv1->length);
  result_bv = bitvector_new (bv0->length);
  for (i = result_bv->size - 1; i >= 0; i--) {
    result_bv->buffer[i] = bv0->buffer[i] ^ bv1->buffer[i];
  }
  return result_bv;
}

bitvector *bitvector_copy (bitvector *bv)
{
  bitvector *result;
  uscalar_t i, size;

  result = bitvector_new (bitvector_length (bv));
  /* new size cannot be larger than the original */
  assert (result->size <= bv->size);
  size = result->size;
  for (i = 0; i < size; i++) {
    result->buffer[i] = bv->buffer[i];
  }
  return result;
}

bool bitvector_is_zeros (bitvector *bv)
{
  uscalar_t length, size, i, j;
  assert (bv != NULL);
  assert (bitvector_length (bv) != 0);

  size = bv->size;
  length = bitvector_length (bv);
  for (i = 0, j = 0; length - i >= BITS_IN_INT; i += BITS_IN_INT, j++) {
    assert (j < size);
    if (bv->buffer[j] != 0) return false;
  }
  assert (j == size - 1 || i == length);
  for (; i < length; i++) {
    if (bitvector_get (bv, i)) return false;
  }
  return true;
}

void bitvector_print (bitvector *bv)
{
  uscalar_t i, length;
  assert (bv != NULL);
  assert (bitvector_length (bv) != 0);

  length = bitvector_length (bv);
  for (i = 0; i < length; i++) {
    fprintf (stderr, "%c", bitvector_get (bv, length - i - 1) ? '1' : '0');
  }
  fprintf (stderr, "\n");
}
