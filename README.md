SpecLearn (DOrder)
==================

For any problem with compiling SpecLearn (DOrder),
send an email to rowangithub@yahoo.com
and I will try to help you use our tool.

SpecLearn (DOrder) is a framework that automatically infers useful specifications for
(higher-order) functional programs. SpecLearn (DOrder) finds specifications from 
program tests and outputs inferred specifications as refinement types.

SpecLearn (DOrder) is not dedicated to higher-order functional programs. It can
infer and verify useful properties for stateful programs as well. 

SpecLearn (DOrder) can automatically infer expressive shape specifications 
for arbitrary user-defined inductive data structures, going well beyond the scope of 
competing tools.

SpecLearn (DOrder) is still under very active development. Our next effort is to develop
SpecLearn (Imp), which targets on (imperative) heap-manipulating programs and array programs.

Quick Start
===========

1. The inductive data structure program benchmarks, for Automatically Learning Shape Specifications, are included in ./tests/reachability/ directory.
We can infer and verify specifications involving rich ordering properties of data structures 
(e.g. in AVL insertion function, the in-order relation of the output binary tree preserves the in-order relations of the input binary tree;
in list reversal function, the forward-order relation of the output list is equivalent to the backward-order relation of the input list;
in heap merge function, the parent-child relation of the output heap preserves the parent-child relations of the input heaps). 
We support arbitrary user-defined algebra data types. Examples include AVL tree, Splay tree, Braun tree, Skew heap, Treap, etc.

		To try an example, run ./msolve.py -no_hoflag -reachability ./tests/reachability/binarytree.ml or
		
							   ./msolve.py -no_hoflag -reachability ./tests/reachability/avl2.ml or
							
						 	   ./msolve.py -no_hoflag -reachability ./tests/reachability/redblackset.ml

2. For the above benchmarks, we also support the inference and verification of shape-data specifications. For example, we can infer and verify functional
correctness specifications for classic list sorting algorithms (e.g. quicksort, mergesort and heapsort) or balanced tree structures (e.g. AVL and Redblack),
proving lists are correctly sorted or trees correctly satisfy BST properties.
Running our tool using the above commands will also display all the shape-data specifications for the examples.

3. In addition to the above ordering properties, SpecLearn (DOrder) can also infer and verify inductive numeric specifications for data structures. For example,
we can infer and verify functional correctness specifications for balanced tree structures (e.g. AVL and Redblack), proving trees can be correctly balanced in
the data structure implementations. The corresponding inductive data structure benchmarks are included in ./tests/dml/ directory. 
	
		To try an example, run ./msolve.py -no_hoflag ./tests/dml/bdd.ml or
		 					   
							   ./moslve.py -no_hoflag ./tests/dml/set.ml

4. SpecLearn (DOrder) not only handles data structure programs, but also can be used to infer specifications for numeric programs.
	The _loop_ (numeric) program benchmarks are included in ./tests/folprograms/ directory.

        To try an example, run ./msolve.py -no_hoflag ./tests/folprograms/misc/popl07.ml

	The _recursive_ (numeric) program benchmarks are included in ./tests/recursive/ directory. 

		To try an example, run ./msolve.py -no_hoflag ./tests/recursive/fibonacci01.ml


5. Importantly, SpecLearn (DOrder) also support _high-order_ functions. The higher-order (numeric) program benchmarks are included 
in ./tests/mochi/ ./tests/lists/ and ./tests/popl13/ directories.

		To try an example, run ./msolve.py -hoflag ./tests/mochi/ainit.ml

We are currently developing SpecLearn (Imp) for checking array intensive programs. Our goal is to infer and verify functional correctness specifications
(e.g. an array reversal function indeed reverses an input array). The evaluation on benchmarks under ./tests/array/ is not ready for the moment.


Compiling SpecLearn (DOrder)
================

SpecLearn (DOrder) requires OCaml 3.12 and Z3 4.3.

Currently, the source code of SpecLearn (DOrder) can only be complied and run in Mac OS 
(with a proper Z3 distribution, SpecLearn (DOrder) can run in Linux).

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

Benchmark programs are included in tests/.

1. Be sure that the files in external/Z3/lib are in your library
   path!  One way to do this is to run, from the top directory,

        export DYLD_LIBRARY_PATH="external/z3/lib/:$DYLD_LIBRARY_PATH"

2. By default, the tool does not infer ordering shape specifications. To infer shape 
specifications on top of ordering and containment properties for data structure programs, run

		./msolve.py -no_hoflag -reachability [ML source file]


3. To check high-order functional programs, run
       
		./msolve.py -hoflag [ML source file]
			
		
4. If you do not want the support for higher-order functions (for performance improvements), run

		./msolve.py -no_hoflag [ML source file]
		
		
5. If you are interested in the performance of our tool in learning numeric invariants over both pure
numeric programs and data structure programs, run (with -no_hoflag)
		
		./msolve.py -no_hoflag [ML source file]

You can observe the inferred function specifications from the terminal (standard output).