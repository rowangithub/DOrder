SpecLearn (DOrder)
==================

SpecLearn is a tool that automatically infers useful invariants for
(higher-order) programs. SpecLearn finds invariants from program samples
and represents invariants into refinement types.

SpecLearn is not dedicated to higher-order functional programs. It can
infer and verify useful properties for array programs. 

A recent extension of SpecLearn is able to prove around 200 loop programs.

SpecLearn is still under very active development. Our next effort is
to enable SpecLearn to verify (imperative) heap-manipulating programs.

Z3
===

SpecLearn has recently chosen Z3 as its backend decision procedure.

1. Be sure that the files in external/Z3/lib are in your library
   path!  One way to do this is to run, from the top directory,

        export DYLD_LIBRARY_PATH="external/z3/lib/:$DYLD_LIBRARY_PATH"

Virtual Machine
===========

An old version of the virtual machine of SpecLearn is available at <a href="https://www.dropbox.com/s/jm3obb9zmq0m9y1/SpecLearn.ova?dl=0">this link</a>.

We will update the VM to reflect our progress soon.

Notes for Reviewers
===========
The loop (numeric) program benchmarks are included in ./tests/folprograms/ directory.

The recursive (numeric) program benchmarks are included in ./tests/recursive/ directory and ./tests/dml/. 
The latter directory contains benchmarks for learning numeric properties over data structure programs.

The higher-order program benchmarks are included in ./tests/mochi/ ./tests/lists/ and ./tests/popl13/ directories.

The data structure program benchmarks, for DOrder implementation, are included in ./tests/reachability/ directory.

At this stage, we are improving the algorithms for checking array programs.
As a result, the evaluation on benchmarks under ./tests/array/ is not stable.
However, this part is not included in the paper, and the rest of the directories 
are also just for our own research purpose.

Compiling SpecLearn
================

SpecLearn requires OCaml 3.12 and Z3.

Currently, the source code of SpecLearn can only be complied and run in Mac OS.

The tool has some difficulties to be compatible with OCaml 4.0+. We will improve our code soon.

To compile SpecLearn, from the top directory:

        Run make libs && make


Running SpecLearn
==============

Example programs are included in tests/.

1. Be sure that the files in external/Z3/lib are in your library
   path!  One way to do this is to run, from the top directory,

        export DYLD_LIBRARY_PATH="external/z3/lib/:$DYLD_LIBRARY_PATH"



2. To check high-order functional programs, run
       
		./msolve.py -hoflag [ML source file]
		
		
		
3. If you do not want the support for higher-order functions (for performance improvements), run

		./msolve.py -no_hoflag [ML source file]
		

		
4. By default, the tool does not check order and reach properties, to infer shape specifications on top
of order and reach properties for data structure programs, run
		
		./msolve.py -no_hoflag -reachability [ML source file] 
		
		
		
5. If you are interested in the performance of our tool in learning numeric invariants over both pure
numeric programs and data structure programs, run (with -no_hoflag)
		
		./msolve.py -no_hoflag [ML source file]

You can observe the inferred function type from the terminal (standard output).