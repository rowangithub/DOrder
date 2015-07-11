#EXEC      = minisat
#DEPDIR    = mtl utils

#include $(MROOT)/mtl/template.mk

SOURCES = $(shell ls *.cc)

OBJS = $(SOURcES: .cc=.O)

default: $(OBJS)
	g++ -O2 -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS -c -fmessage-length=0 -I../ Solver.cc