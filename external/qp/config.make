SHELL        = /bin/sh

C_C          = gcc
CPP_C        = g++
ifdef GPROF
C_CPP_FLAGS = -pg -O3
else
ifndef DEBUG
C_CPP_FLAGS  = -O3
else
C_CPP_FLAGS  = -g
endif
endif
C_LD         = $(C_C)
CPP_LD       = $(CPP_C)

C_YACC       = bison
C_LEX        = flex

AR           = ar
RANLIB       = ranlib

OCAML_C      = ocamlc
OCAML_OPT_C  = ocamlopt
ifdef GPROF
OCAML_C_FLAGS     = -g -w px
OCAML_OPT_C_FLAGS = -p -unsafe -noassert -w px -dtypes
OCAML_LD     = $(OCAML_C) -g
OCAML_OPT_LD = $(OCAML_OPT_C) -p
else
ifndef DEBUG
OCAML_C_FLAGS     = -w px
OCAML_OPT_C_FLAGS = -unsafe -noassert -w px -dtypes
OCAML_LD     = $(OCAML_C) -g
OCAML_OPT_LD = $(OCAML_OPT_C)
else
OCAML_C_FLAGS     = -g -w px -dtypes
OCAML_OPT_C_FLAGS = -w px -dtypes
OCAML_LD     = $(OCAML_C) -g
OCAML_OPT_LD = $(OCAML_OPT_C)
endif
endif
OCAML_MKTOP  = ocamlmktop
OCAML_CP     = ocamlcp
OCAML_DEP    = ocamldep
OCAML_LEX    = ocamllex
OCAML_YACC   = ocamlyacc

OCAML_C_CPP_INC = -I $(shell $(OCAML_C) -v | tail -1 | sed -e \
                             's/^Standard library directory: //')
