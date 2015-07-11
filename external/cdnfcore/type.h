/*****************************************************************************
 *
 * 
 * Author: Bow-Yaw Wang
 * Copyright reserved
 *****************************************************************************/

#ifndef _TYPE_H_
#define _TYPE_H_

#ifdef __i386__
typedef int scalar_t;
typedef unsigned int uscalar_t;
#endif

#ifdef __x86_64__
typedef long scalar_t;
typedef unsigned long uscalar_t;
#endif

typedef void *pointer_t;

#endif
