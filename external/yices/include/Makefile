# Change this to the path to Yices on your system
YICES_BASE = ../../

# Change this to the directory where OCaml libs should
# be installed
OCAML_LIB_DIR = $(YICES_BASE)/lib

LIBDIR = $(YICES_BASE)/lib/
INCLUDEDIR = $(YICES_BASE)/include/
BINDIR = $(YICES_BASE)/bin/

OBJS = oyices.cma
C_OBJS = liboyices.a
NATIVE_OBJS = oyices.cmxa oyices.a
LIB = oyices.cmo oyices.cmx oyices.cmi $(OBJS) $(NATIVE_OBJS) $(C_OBJS)

INTERNAL_OBJS = swig.cmo oyicesunsafe.cmo
INTERNAL_LIB = swig.cmi oyicesunsafe.cmi $(INTERNAL_OBJS)

TOPLEVEL = topoyices

C_SOURCE = liboyices.c
EXTERNAL_SOURCES = oyices.ml oyices.mli
INTERNAL_SOURCES = oyicesunsafe.ml oyicesunsafe.mli $(C_SOURCE)
INTERFACE_SOURCE = $(INCLUDEDIR)/oyices.i

CCOPTS = "-L$(LIBDIR)" -cclib -lyices -cclib -lgmp


.PHONY: default
default: $(LIB)
	mv $(LIB) $(LIBDIR)

%.cmxa: $(subst .cmo,.cmx,$(INTERNAL_OBJS)) %.cmx
	ocamlopt -a $^ -o $@

%.cma: $(INTERNAL_OBJS) %.cmo
	ocamlc -a $^ -o $@

%.cmx: %.ml %.cmi
	ocamlopt -ccopt $(CCOPTS) -c $<

# This is not a true dependency, but my make-fu is insufficient
# and ocaml is unhappy if you have an mli with no cmi around
# when compiling a module
%.cmo: %.ml %.cmi
	ocamlc -ccopt $(CCOPTS) -c $<

%.cmi: %.mli
	ocamlc -c $<

swig.ml:
	swig -outdir ./ -ocaml -co swig.ml

swig.mli:
	swig -outdir ./ -ocaml -co swig.mli

$(EXTERNAL_SOURCES): $(INTERNAL_LIB) oyices.ml oyices.mli
	

.PHONY: safeinterface
safeinterface: $(INTERNAL_LIB)
	$(INCLUDEDIR)/make-safe-interface.py $(INTERFACE_SOURCE) oyicesunsafe oyices

$(INTERNAL_SOURCES): swigsource
	

.PHONY: swigsource
swigsource: $(INTERFACE_SOURCE)
	swig -outdir ./ -o $(C_SOURCE) -ocaml $(INTERFACE_SOURCE)

%.a: %.o
	ar rc $@ $<
	ranlib $@

%.o: %.c
	ocamlc -ccopt "-I$(INCLUDEDIR) -o $@" $<

.PHONY: toplevel
toplevel: $(OBJS) $(C_OBJS)
	ocamlmktop -ccopt $(CCOPTS) -custom $(C_OBJS) -o $(TOPLEVEL) $(OBJS)
	mv $(TOPLEVEL) $(BINDIR)

.PHONY: install
install: default
	cp $(LIBDIR)/* $(OCAML_LIB_DIR)

.PHONY: clean
clean:
	rm -f *.c *.mli *.ml *.cmo *.cmi *.cma *.a *.o

.PHONY: distclean
distclean: clean
	cd $(LIBDIR); rm -f $(LIB)
	cd $(BINDIR); rm -f $(TOPLEVEL)

