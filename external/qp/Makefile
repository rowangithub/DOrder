ifndef QPHOME
QPHOME=$(shell pwd)
export QPHOME
endif


ifdef RELEASE
NATIVECAML := 1
EXE := opt
else
EXE := byte
endif
# GPROF = 1
DEBUG = 1
# OCAML_BYTECODE_CINTF = 1

# Configuration

include $(QPHOME)/config.make

CPPFLAGS := $(C_CPP_FLAGS) -Wall

ifdef PROFILING
OCAML_C = $(OCAML_CP) -p f
endif

OCAML_LIB      = libqp
OCAML_LIB_OBJ  = qpVector qpErrormsg \
		 qpMisc \
		 qpAst \
		 qpDag \
		 qpArch \
		 cong \
		 qpEquality \
		 qpDiff \
		 qProver \
		 qpParse qpLex 

OCAML_EXE_OBJ  = $(OCAML_LIB_OBJ) main
OCAML_EXE      = qp

OCAML_CINTF_OBJ = $(OCAML_LIB_OBJ) 

C_CPP_INCLUDES = -I $(QPHOME)/include $(OCAML_C_CPP_INC)
OCAML_INCLUDES =# -I $(YICESHOME)/lib/ 
#OCAML_INCLUDES = -I $(FOCIHOME)/vampyre/include

C_CPP_LD_FLAGS = -L $(QPHOME)/lib 
C_CPP_LD_LIBS  = -lnums -lstr -lunix -lutil 

OCAML_LD_FLAGS = -I $(QPHOME)/lib 

OCAML_LD_LIBS  = nums str unix 
MODULES        = $(OCAML_EXE_OBJ) 
MLYS           = qpParse.mly  
MLLS           = qpLex.mll

TODO           = $(OCAML_EXE).top $(OCAML_EXE).byte $(OCAML_EXE).opt \
                 $(OCAML_LIB).cma $(OCAML_LIB).cmxa 

all:: bin lib 

bin: $(TODO)
	mkdir -p $@
	cd bin; ln -fs ../$(OCAML_EXE).{byte,opt} .

lib: $(TODO)
	mkdir -p $@
	cd lib; ln -fs ../{$(OCAML_LIB).{cma,cmxa,a}} .

clean: default-clean

distclean: clean
	rm -f *.cmi *.annot *.a $(TODO) $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) \
	$(MLLS:%.mll=%.ml)

srcdist:		
	cd ..; tar czvf qp.tar.gz ./qp/{*.ml,*.mli,*.mly,*.mll,Makefile,*.make} ./qp/README ./qp/test/* 

bindist:		
	tar czvhf qp-1.0b-i386.tar.gz README LICENSE bin lib 

.SECONDARY: $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) $(MLLS:%.mll=%.ml)

.PHONY : all clean distclean

# Implicit rules

include $(QPHOME)/rules.make

# Dependencies

-include $(MODULES:%=.%.ml.depend) $(MODULES:%=.%.mli.depend)
