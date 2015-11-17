SpecLearn (DOrder)
==================

For any problem with compiling SpecLearn (DOrder),
send an email to rowangithub@yahoo.com
and I will try to help you use our tool.

SpecLearn (DOrder) is a framework that automatically infers useful specifications for
(higher-order) functional programs. SpecLearn (DOrder) finds specifications from 
program tests and represents inferred specifications as refinement types.

SpecLearn (DOrder) is not dedicated to higher-order functional programs. It can
infer and verify useful properties for stateful programs as well. 

SpecLearn (DOrder) can automatically infer expressive shape specifications 
for arbitrary user-defined inductive data structures, going well beyond the scope of 
competing tools.

SpecLearn (DOrder) is still under very active development. Our next effort is to develop
SpecLearn (Imp), which targets on (imperative) heap-manipulating programs and array programs.

Quick Start
===========

1. The loop (numeric) program benchmarks are included in ./tests/folprograms/ directory.

        To try an example, run ./msolve.py -no_hoflag ./tests/folprograms/misc/popl07.ml


2. The recursive (numeric) program benchmarks are included in ./tests/recursive/ directory and ./tests/dml/ directory. 
The latter directory contains benchmarks for learning numeric specifications for inductive data types.

		To try an example, run ./msolve.py -no_hoflag ./tests/dml/bdd.ml


3. The higher-order (numeric) program benchmarks are included in ./tests/mochi/ ./tests/lists/ and ./tests/popl13/ directories.

		To try an example, run ./msolve.py -hoflag ./tests/mochi/ainit.ml


4. The inductive data structure program benchmarks, for Data-Driven Shape Specification Inference, are included in ./tests/reachability/ directory.
We can infer and verify specifications involving rich containment and ordering properties of data structures 
(e.g. in-order relation of a binary tree). 

		To try an example, run ./msolve.py -reachability -no_hoflag ./tests/reachability/binarytree.ml or
							   ./msolve.py -no_hoflag -reachability ./tests/reachability/avl2.ml or
						 	   ./msolve.py -no_hoflag -reachability ./tests/reachability/redblackset.ml
		

At this stage, we are developing SpecLearn (Imp) for checking array programs.
As a result, the evaluation on benchmarks under ./tests/array/ is not stable.
The rest of the directories are also just for our own research purposes.

Compiling SpecLearn (DOrder)
================

SpecLearn requires OCaml 3.12 and Z3 4.3.

Currently, the source code of SpecLearn can only be complied and run in Mac OS 
(with a proper Z3 distribution, SpecLearn can run in Linux).

The tool has some troubles to be compatible with OCaml 4.0+. We will improve our code soon.

To detect whether your machine supports SpecLearn (DOrder), 
		
		Run ./configure

In order to make sure that you have Z3 ready for the tool, we require users to manually 

		Go into external/z3/ocaml, and run ./build-lib.sh /usr/local/lib/ocaml/
		
If you encounter any problem, please follow the ReadMe provided under external/z3/ocaml
to install Z3. You are also required to put libz3.dylib (you need to compile Z3 to get this
file) under external/z3/lib.


Then, to compile SpecLearn (DOrder), from the top directory:

        Run make libs && make

Note that our makefile assumes that CamlIDL is installed under /usr/local/lib/ocaml. You
should modify the configurations in the makefile if necessary.

Running SpecLearn (DOrder)
==============

Example programs are included in tests/.

1. Be sure that the files in external/Z3/lib are in your library
   path!  One way to do this is to run, from the top directory,

        export DYLD_LIBRARY_PATH="external/z3/lib/:$DYLD_LIBRARY_PATH"



2. To check high-order functional programs, run
       
		./msolve.py -hoflag [ML source file]
		
		
		
3. If you do not want the support for higher-order functions (for performance improvements), run

		./msolve.py -no_hoflag [ML source file]
		
		
4. If you are interested in the performance of our tool in learning numeric invariants over both pure
numeric programs and data structure programs, run (with -no_hoflag)
		
		./msolve.py -no_hoflag [ML source file]
		
	
5. By default, the tool does not try to infer ordering and containment specifications, to infer shape 
specifications on top of ordering and containment properties for data structure programs, run

		./msolve.py -no_hoflag -reachability [ML source file]

You can observe the inferred function type from the terminal (standard output).