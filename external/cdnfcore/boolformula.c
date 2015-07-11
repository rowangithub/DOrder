/*****************************************************************************
 *
 * 
 * Author: Yu-Fang Chen & Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "type.h"
#include "bitvector.h"
#include "vector.h"
#include "boolformula.h"
//#define DEBUG

/* variable id's start from 1 */

const lit NOT_A_LITERAL = 0;

inline lit boolformula_lit_from_var (var v)
{
  assert (v > 0);
  return v;
}

inline var boolformula_var_from_lit (lit l)
{
  assert (l != 0);
  return l > 0 ? l : -l;
}

inline lit boolformula_lit_complement (lit l)
{
  assert (l != 0);
  return -l;
}

inline bool boolformula_positive_lit (lit l)
{
  assert (l != 0);
  return l > 0;
}

/*
 * vector of length 0 is the unit of disjunction (F)
 */
inline boolformula_t *boolformula_disjunction_unit (void)
{
	boolformula_t* ret=malloc(sizeof(boolformula_t));
	ret->d.v=vector_new(0);
	ret->t=disjunct;
	ret->ref=1;
	return ret;
}

inline boolformula_t *boolformula_disjunction_new (uscalar_t length)
{
	boolformula_t* ret=malloc(sizeof(boolformula_t));
	ret->d.v=vector_new(length);
	ret->t=disjunct;
	ret->ref=1;
	return ret;
}

/*
 * vector of length 0 is the unit of disjunction (T)
 */
inline boolformula_t *boolformula_conjunction_unit (void)
{
	boolformula_t* ret=malloc(sizeof(boolformula_t));
	ret->d.v=vector_new(0);
	ret->t=conjunct;
	ret->ref=1;
	return ret;
}

inline boolformula_t *boolformula_conjunction_new (uscalar_t length)
{
	boolformula_t* ret=malloc(sizeof(boolformula_t));
	ret->t=conjunct;
	ret->d.v=vector_new(length);
	ret->ref=1;
	return ret;
}

inline boolformula_t *boolformula_literal_new (lit l)
{
	boolformula_t* ret=malloc(sizeof(boolformula_t));
	ret->d.l=l;
	ret->t=literal;
	ret->ref=1;
	return ret;
}

inline boolformula_t *boolformula_add (boolformula_t *f,
		boolformula_t *g)
{
	assert(f->t!=literal);
	vector_add (f->d.v, g);
	g->ref++;
	return f;
}

inline boolformula_t *boolformula_set (boolformula_t *f, uscalar_t idx,
		boolformula_t *g)
{
	assert(f->t!=literal);
	vector_set (f->d.v, idx, g);
	g->ref++;
	return f;
}


inline void boolformula_free (boolformula_t *f)
{
	if(f->ref>1){
		f->ref--;
		return;
	}
	int num_of_subformulae;
	if(f->t!=literal){
		  num_of_subformulae = vector_length (f->d.v);
		  int i;
		  for (i = 0; i < num_of_subformulae; i++) {
			  boolformula_free((boolformula_t*)vector_get (f->d.v, i));
		  }
		  vector_free (f->d.v);
	}
	free(f);
}

inline boolformula_t* boolformula_neg (boolformula_t * f){
	int num_of_subformulae;
	int i;
	switch(f->t){
		case disjunct:
			f->t=conjunct;
			num_of_subformulae = vector_length (f->d.v);
			for (i = 0; i < num_of_subformulae; i++) {
			  boolformula_neg((boolformula_t*)vector_get (f->d.v, i));
			}
			break;
		case conjunct:
			f->t=disjunct;
			num_of_subformulae = vector_length (f->d.v);
			for (i = 0; i < num_of_subformulae; i++) {
			  boolformula_neg((boolformula_t*)vector_get (f->d.v, i));
			}
			break;
		case literal:
			f->d.l=-f->d.l;
			break;
	}
	return f;
}




inline void add_clauses_to_boolformula(boolformula_t* ret, boolformula_t * f, lit* next_fresh){
	int i;
	boolformula_t* cur_neg=boolformula_literal_new(boolformula_lit_complement(*next_fresh));
	boolformula_t* dis, *temp;
	switch(f->t){
	case conjunct:
		for(i=0;i<vector_length (f->d.v);i++){
			dis=boolformula_disjunction_new(2);
			boolformula_add(ret, dis);

			boolformula_set(dis, 0,cur_neg);
			if(((boolformula_t*)vector_get(f->d.v,i))->t==literal){
				temp=boolformula_literal_new(((boolformula_t*)vector_get(f->d.v,i))->d.l);
				boolformula_set(dis, 1, temp);
				boolformula_free(temp);
			}else{
				(*next_fresh)++;
				temp=boolformula_literal_new(*next_fresh);
				boolformula_set(dis, 1, temp);
				boolformula_free(temp);
				add_clauses_to_boolformula(ret, vector_get(f->d.v,i), next_fresh);
			}
			boolformula_free(dis);
		}
		break;
	case disjunct:
		dis=boolformula_disjunction_new(1+vector_length (f->d.v));
		boolformula_add(ret, dis);
		boolformula_set(dis, 0, cur_neg);
		for(i=0;i<vector_length (f->d.v);i++){
			if(((boolformula_t*)vector_get(f->d.v,i))->t==literal){
				temp=boolformula_literal_new(((boolformula_t*)vector_get(f->d.v,i))->d.l);
				boolformula_set(dis,i+1, temp);
				boolformula_free(temp);
			}else{
				(*next_fresh)++;
				temp=boolformula_literal_new(*next_fresh);
				boolformula_set(dis, i+1,temp);
				boolformula_free(temp);
				add_clauses_to_boolformula(ret, vector_get(f->d.v,i), next_fresh);
			}
		}
		boolformula_free(dis);

		break;
	case literal:
		printf("Something wrong\n");
		exit(13);
		break;
	}
	boolformula_free(cur_neg);

}


inline boolformula_t *boolformula_to_cnf (boolformula_t * f, scalar_t num_var){
	scalar_t next_fresh=num_var+1;
	int i;
	boolformula_t* ret=NULL;
	boolformula_t* cur;
	boolformula_t* cur_neg;
	boolformula_t* temp;

	switch(f->t){
	case literal:
		ret=boolformula_copy(f);
		break;
	case conjunct:
	case disjunct:
		ret=boolformula_conjunction_unit();
		cur=boolformula_literal_new(next_fresh);
		boolformula_add(ret, cur);
		add_clauses_to_boolformula(ret, f, &next_fresh);
		boolformula_free(cur);
		break;
	}
	return ret;
}

inline enum type boolformula_get_type (boolformula_t * f){
        assert (f->t == literal || f->t == conjunct || f->t == disjunct);
	return f->t;
}

inline uscalar_t boolformula_get_length (boolformula_t * f){
	assert(f->t == conjunct || f->t == disjunct);
        return vector_length (f->d.v);
}

inline boolformula_t *boolformula_get_subformula (boolformula_t * f, uscalar_t idx){
	assert(f->t == conjunct || f->t == disjunct);
	return vector_get(f->d.v, idx);
}

inline lit boolformula_get_value (boolformula_t * f){
	assert(f->t==literal);
	return f->d.l;
}


void boolformula_print (boolformula_t *f)
{
	switch(f->t){
		case disjunct:
			  if (vector_length (f->d.v) == 0) {
			    fprintf (stderr, "{ F }");
			  } else {
			    uscalar_t i;
			    fprintf (stderr, "{ ");
			    for (i = 0; i < vector_length (f->d.v)-1; i++) {
			      boolformula_print (vector_get (f->d.v, i));
			      fprintf (stderr, " | ");
			    }
			    assert (i == vector_length (f->d.v)-1);
			    boolformula_print (vector_get (f->d.v, vector_length (f->d.v)-1));
			    fprintf (stderr, " }");
			  }
			break;
		case conjunct:
			  if (vector_length (f->d.v) == 0) {
			    fprintf (stderr, "{ T }");
			  } else {
			    uscalar_t i;
			    fprintf (stderr, "{ ");
			    for (i = 0; i < vector_length (f->d.v)-1; i++) {
			      boolformula_print (vector_get (f->d.v, i));
			      fprintf (stderr, " & ");
			    }
			    assert (i == vector_length (f->d.v)-1);
			    boolformula_print (vector_get (f->d.v, vector_length (f->d.v)-1));
			    fprintf (stderr, " }");
			  }
			break;
		case literal:
		      fprintf (stderr, "%ld", f->d.l);
			break;
	}
}

scalar_t boolformula_num_of_var(boolformula_t* f){

	scalar_t cur=0, temp;
	int i;
	switch(f->t){
	case literal:
		return abs(f->d.l);
		break;
	default:
		for(i=0;i<f->d.v->length;i++){
			temp=boolformula_num_of_var((boolformula_t*)vector_get(f->d.v, i));
			cur=cur > temp?cur:temp;
		}
		break;
	}
	return cur;

}



inline boolformula_t *boolformula_copy(boolformula_t * f){
	boolformula_t* ret=NULL, *temp;
	int i;
	switch(f->t){
	case conjunct:
		ret=boolformula_conjunction_new(f->d.v->length);
		for(i=0;i<f->d.v->length;i++){
			temp=boolformula_copy(vector_get(f->d.v,i));
			boolformula_set(ret,i, temp);
			boolformula_free(temp);
		}
		break;
	case disjunct:
		ret=boolformula_disjunction_new(f->d.v->length);
		for(i=0;i<f->d.v->length;i++){
			temp=boolformula_copy(vector_get(f->d.v,i));
			boolformula_set(ret,i, temp);
			boolformula_free(temp);
		}
		break;
	case literal:
		ret=boolformula_literal_new(f->d.l);
		break;
	}
	assert(f->t==ret->t);

	return ret;
}
