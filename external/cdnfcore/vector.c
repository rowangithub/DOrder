/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include "type.h"
#include "vector.h"

#define MIN_SIZE 8

const uscalar_t INITIAL_CAPACITY = 8;

static inline uscalar_t length_to_size (uscalar_t length)
{
  uscalar_t result;

  for (result = MIN_SIZE; result < length; result = (result * 3) >> 1)
    ;
  assert (result >= length);
  return result;
}

vector *vector_new (uscalar_t length)
{
  vector *result;

  result = (vector *) malloc (sizeof (vector));
  assert (result != NULL);
  result->length = 0;
  result->size = 0;
  result->buffer = NULL;
  vector_resize (result, length);
  return result;
}

void vector_free (vector *vec)
{
  assert (vec != NULL);
  if (vec->buffer != NULL) free (vec->buffer);
  vec->buffer = NULL;
  free (vec);
}

void vector_resize (vector *vec, uscalar_t new_length)
{
  uscalar_t new_size;

  assert (vec != NULL);
  new_size = length_to_size (new_length);

  if (new_size != vec->size) {
    vec->buffer = 
      (pointer_t *) realloc (vec->buffer, new_size * sizeof (pointer_t));
    assert (vec->buffer != NULL);
    vec->size = new_size;
  }
  vec->length = new_length;
}

inline uscalar_t vector_length (vector *vec)
{
  assert (vec != NULL);
  return vec->length;
}

inline pointer_t vector_get (vector *vec, uscalar_t idx)
{
  assert (idx < vec->length);
  return vec->buffer[idx];
}

inline void vector_set (vector *vec, uscalar_t idx, pointer_t elt)
{
  assert (idx < vec->length);
  vec->buffer[idx] = elt;
}

inline void vector_add (vector *vec, pointer_t elt)
{
  vector_resize (vec, vec->length + 1);
  vector_set (vec, vec->length - 1, elt);
}
