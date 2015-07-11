/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#ifndef _CDNF_H_
#define _CDNF_H_

#define CDNF  0
#define CDNF_Plus  1
#define CDNF_PlusPlus 2
#define CDNF_Plus3 3

boolformula_t *learn (void *info, uscalar_t num_vars, 
                      membership_t membership, membership_t comembership, 
                      equivalence_t equivalence, int mode);
#endif

