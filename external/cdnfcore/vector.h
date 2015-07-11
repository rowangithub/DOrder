/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#ifndef _LIBCDNFP_VECTOR_H_
#define _LIBCDNFP_VECTOR_H_

extern const uscalar_t INITIAL_CAPACITY;

typedef struct {
  uscalar_t length; /* length of vector */
  uscalar_t size;   /* size of buffer */
  pointer_t *buffer;
} vector;

vector *vector_new (uscalar_t length);
void vector_free (vector *vec);
void vector_resize (vector *vec, uscalar_t new_length);
inline uscalar_t vector_length (vector *vec);
inline pointer_t vector_get (vector *vec, uscalar_t idx);
inline void vector_set (vector *vec, uscalar_t idx, pointer_t elt);
inline void vector_add (vector *vec, pointer_t elt);

#endif
