/*****************************************************************************
 *
 * 
 * Author: Yu-Fang Chen & Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/


#ifndef _BOOLFORMULA_H_
#define _BOOLFORMULA_H_


typedef uscalar_t var;
typedef scalar_t lit;
extern const lit NOT_A_LITERAL;

enum type{disjunct, conjunct, literal};

union data {
    lit l;
    vector* v;
};

typedef struct {
	uscalar_t ref;
	enum type t;
	union data d;
} boolformula_t;

inline lit boolformula_lit_from_var (var);
inline var boolformula_var_from_lit (lit);
inline lit boolformula_lit_complement (lit);
inline bool boolformula_positive_lit (lit);

inline boolformula_t *boolformula_neg (boolformula_t *);
inline boolformula_t *boolformula_disjunction_new (uscalar_t length);
inline boolformula_t *boolformula_disjunction_unit (void);
inline boolformula_t *boolformula_conjunction_new (uscalar_t length);
inline boolformula_t *boolformula_conjunction_unit (void);
inline boolformula_t *boolformula_literal_new (lit);

inline boolformula_t *boolformula_add (boolformula_t *, boolformula_t *);
inline boolformula_t *boolformula_set (boolformula_t *, uscalar_t idx, boolformula_t *);

inline enum type boolformula_get_type (boolformula_t *);
inline uscalar_t boolformula_get_length (boolformula_t *);
inline boolformula_t *boolformula_get_subformula (boolformula_t *, uscalar_t idx);//for disjunct/disjunct
inline lit boolformula_get_value (boolformula_t *);//for literal


inline void boolformula_free (boolformula_t *);
void boolformula_print (boolformula_t *f);
scalar_t boolformula_num_of_var(boolformula_t* f);

inline boolformula_t *boolformula_to_cnf (boolformula_t *, scalar_t);
inline boolformula_t *boolformula_copy(boolformula_t *);



#endif
