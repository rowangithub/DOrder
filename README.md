DOrder
==================

<a href="https://www.cs.purdue.edu/homes/zhu103/pubs/draft.pdf">
	Link to the paper on Automatically Learning Shape Specifications.</a>
	
<strong>DOrder</strong> is a specification synthesizer written in OCaml that
runs on top of the OCaml compiler. It is capable of synthesizing shape 
specifications for OCaml data structure programs with <em>no</em> user-annotations. 
It only requires a <strong>small</strong> number of <strong>simple tests</strong>
to bootstrap synthesis. 

Below, we provide a guide for fun things you can play with DOrder, with
pointers to the paper for further information.

<h3>DOrder Virtual Machine</h3>

Before directly accessing DOrder's source code, we recommend a <a href="">VM version</a> of DOrder.
DOrder is complied and ready to be played in the VM version.

<h3>DOrder Source Code</h3>

You can git-clone the source code of DOrder:

		git clone https://github.com/rowangithub/DOrder.git
	
System requirements:

1. OCaml 3.12:

	The tool is currently incompatible with OCaml 4.0+. We hope to improve our code in the future.
	The following instructions assume OCaml library is installed under /usr/local/lib/ocaml/, which is
	also the default setting. If not, please make necessary changes according to your machine.
	
2. Z3 4.3:

	DOrder requires Z3 to be installed. Download and install Z3 following all instructions provided 
	<a href="https://github.com/Z3Prover/z3">here</a>. 
	We strongly recommend Z3 4.3. To bind Z3 to DOrder, we require users to manually 

			Go into external/z3/ocaml, and run ./build-lib.sh /usr/local/lib/ocaml/

	Please also put libz3.dylib or libz3.so (these files are available upon success compilation of Z3) 
	under external/z3/lib.
	If any problem is encountered, please follow the ReadMe provided under external/z3/ocaml.
	
3. CamlIDL:	

	CamlIDL can be downloaded from <a href="http://caml.inria.fr/pub/old_caml_site/camlidl/">here</a>.
	
	
To detect whether an operating system supports DOrder, 

			Run ./configure

To compile DOrder, from the top directory:

	        Run make libs && make	
	
To run DOrder, 

1. In MacOS, be sure that the files in external/Z3/lib are in your library path (not required in Ubuntu).  
One way to do this is to run, from the top directory,
	
			export DYLD_LIBRARY_PATH="external/z3/lib/:$DYLD_LIBRARY_PATH"

2. To test whether DOrder is successfully complied, run

			./msolve.py ./tests/recursive/mcCarthy91.ml
	
A precise specification for the well-known mcCarthy91 function should be
displayed. For any other problem with compiling DOrder, 
send an email to zhu103 AT myuniversity.


Overview
===========

Abstractly, DOrder implements a general framework that 
automatically synthesizes useful specifications as _refinement types_
for (higher-order) functional programs from test outcome.

Concretely, DOrder presents a novel automated procedure for discovering
expressive shape specifications for sophisticated functional
data structures. Our approach extracts potential shape
predicates based on the definition of constructors of arbitrary
user-defined inductive data types, and combines these
predicates within an expressive first-order specification language
using a lightweight data-driven <strong>learning</strong> procedure.

Notably, this technique requires no programmer annotations,
and is equipped with a type-based decision procedure to verify
the correctness of discovered specifications. Experimental
results indicate that our implementation is both efficient
and effective, capable of automatically synthesizing sophisticated
shape specifications over a range of complex data
types, going well beyond the scope of existing solutions.


Run the benchmarks from the paper
===========

<h3>Benchmark location:</h3>
		
		./tests/reachability/

<h3>How to:</h3>

1. The inductive data structure program benchmarks, for Automatically Learning Shape Specifications, are included in ./tests/reachability/ directory.
We can infer and verify specifications involving rich ordering properties of data structures 
(e.g. in AVL insertion function, the in-order relation of the output binary tree preserves the in-order relations of the input binary tree;
in list reversal function, the forward-order relation of the output list is equivalent to the backward-order relation of the input list;
in heap merge function, the parent-child relation of the output heap preserves the parent-child relations of the input heaps). 
We support arbitrary user-defined algebra data types. Examples include AVL tree, Splay tree, Braun tree, Skew heap, Treap, etc.

		To try an example, run ./msolve.py -no_hoflag -reachability ./tests/reachability/binarytree.ml or
		
							   ./msolve.py -no_hoflag -reachability ./tests/reachability/avl2.ml or
							
						 	   ./msolve.py -no_hoflag -reachability ./tests/reachability/redblackset.ml

2. We also support the inference and verification of shape-data specifications. For example, we can infer and verify functional
correctness specifications for classic list sorting algorithms (e.g. quicksort, mergesort and heapsort) or 
balanced tree data structure programs (e.g. AVL and Redblack),
proving lists are correctly sorted or trees correctly satisfy BST properties.
Running our tool using the above commands will also display all the synthesized shape-data specifications.


<h3>Assumptions made by DOrder:</h3>

For any data structure program _prog_,
DOrder assumes test inputs to _prog_ are provided in a file <em>prog_harness</em>.
These test inputs can be generated from automated tools like _quickcheck_.
In fact, all test inputs from _harness_ files are in _quickcheck_ style.
They are all very _simple_.
If used to synthesize specifications for some new data structure, 
make sure a new _harness_ file for the data structure is created.
Many existing _harness_ test files in the repository are reusable. 

For example, consider the _heapsort_ program under ./test/reachability.
Its test inputs are described in <em>heapsort_harness</em>, 
which contains the following code:

	let list n = _random a sequence of numbers_

	let main () = 
		let _ = fprintf outch "env:newtest\t\n" in
		heapsort (list 15)
	let _ = main ()

The line "let _ = fprintf outch "env:newtest\t\n" in" is used to tell
DOrder to collect input-output behaviors of the function below (e.g. _heapsort_).

<h3>DOrder output explanation:</h3> 

Synthesized specifications are boolean combinations of a set of atomic 
predicates inferred _per-datatype_. For example, consider the data type 
_heap_ provided in the _heapsort_ program.

	type 'a heap = 
		| E 
		| T of int * 'a * 'a heap * 'a heap
		
A number of atomic predicates are created from this data type
(following Section.2 of the paper).
We first consider possible containment predicates for trees.

		reach (h, u) represents a certain value u is present in a tree h.
		
A more interesting predicate class is one that establishes
ordering relations between two elements of a data structure,
u and v. Recall that in the heap definition only T constructors
contain values. However, since T contains two
inductively defined subtrees, there are several cases to consider
when establishing an ordering relation among values
found within a tree h. We use link (h, t, i, j, u, v) to represent
the ordering relations u is in the i-th component and v is in the j-th
component of constructor T in h.
For example, if we are interested in cases where the
value u appears “before” (according to a specified order) v,
we could either have that: 

		(i) the value v occurs in the first (left) subtree from a tree node containing u, 
		described by the notation link (h, t, 1, 2, u, v), 

		(ii) the value v occurs in the second (right) subtree, described by the notation link (h, t, 1, 3, u, v),

		(iii) both values are in the tree, but u is found in a subtree that is disjoint from the subtree where v occurs. 
		Suppose there exists a node whose first subtree contains u and whose second subtree contains v. 
		This is denoted as link (h, t, 2, 3, u, v). 
		
The symmetric cases are obvious,
and we do not describe them. Notice that in this description
we have exhausted all possible relations between any two
values in a tree. 

_Simplification:_ Given a predicate link (h, t, i, j, u, v), 
to improve _readability_ of DOrder, if the
i-th component of constructor T is the only argument of T that is
not an inductive data type (e.g. the first argument of T in 'a heap 
definition given above), we simplify the predicate to link (h, t, j, u, v);
if both i-th component and j-th component are of inductive data type
(e.g. the second and third arguments of T in 'a heap), we simplify
the predicate to link (h, t, ij, u, v). 

After synthesizing atomic predicates from datatype definition, 
DOrder synthesizes specifications for data structure functions. 
Consider the _merge_ function in _heapsort_,

	let rec merge h1 h2 =
		match h1, h2 with 
		| h1, E -> h1 
		| E, h2 -> h2 
		| (T(rk1, x, a1, b1)), (T(rk2, y, a2, b2)) -> 
			if x >= y then 
				t x a1 (merge b1 h2) 
			else 
				t y a2 (merge h1 b2)
	
By learning from test outcome, the following specification is synthesized:
	
	function merge with type h1: {'a heap | some type omitted ... } 
		-> h2: {'a heap | sometype omitted ... }
		-> {'a heap |                              			
				forall (u v ). ((-. link (V, t, 1, 2, u, v)) or 
				link (h2, t, 1, 2, u, v) or
				link (h2, t, 1, 3, u, v) or
				(link (h1, t, 1, 3, u, v) or 
				link (h1, t, 1, 2, u, v)) or
				((reach (h2, u)) and (reach (h1, v))) or
				((reach (h2, v)) and (reach (h1, u)))) /\ ...}

In the result type, V represents the result heap. The given specification states that
the parent-child relation (e.g. link (V, 1, 2, u, v) where u and v are free) between 
elements contained in the result heap preserves their parent-child relation 
(e.g. link (h2, t, 1, 3, u, v)) in the input heap h1 and h2.

DOrder also outputs _shape-data_ specifications. For example, for the _heapsort_ function,
the following specification is synthesized:

	function heapsort with type ls: 'a list ->
        {'a list | forall (u v ). ((-. link (V, cons, 0, 1, u, v)) or  (v <= u)) /\ ...}

In the result type, we see that the output list is correctly sorted, where _cons_
represent the Cons data type constructor of list.
        

Learning other specifications beyond the paper
===========

3. In addition to the above ordering properties, DOrder can also infer and verify inductive numeric specifications for data structures. For example,
we can infer and verify functional correctness specifications for balanced tree structures (e.g. AVL and Redblack), proving trees can be correctly balanced in
the data structure implementations. The corresponding inductive data structure benchmarks are included in ./tests/dml/ directory. 
	
		To try an example, run ./msolve.py -no_hoflag ./tests/dml/bdd.ml or
		 					   
							   ./moslve.py -no_hoflag ./tests/dml/set.ml

4. DOrder not only handles data structure programs, but also can be used to infer specifications for numeric programs.
	The _loop_ (numeric) program benchmarks are included in ./tests/folprograms/ directory.

        To try an example, run ./msolve.py -no_hoflag ./tests/folprograms/misc/popl07.ml

	The _recursive_ (numeric) program benchmarks are included in ./tests/recursive/ directory. 

		To try an example, run ./msolve.py -no_hoflag ./tests/recursive/fibonacci01.ml


5. DOrder supports _high-order_ functions. The higher-order (numeric) program benchmarks are included 
in ./tests/mochi/ ./tests/lists/ and ./tests/popl13/ directories.

		To try an example, run ./msolve.py -hoflag ./tests/mochi/ainit.ml
		
6. DOrder can synthesize quantified array invariants.
	The _array_ program benchmarks are included in ./tests/array/
	
		To try an example, run ./msolve.py -inv -effect ./tests/array/a_quicksort_partition.ml


Summary: DOrder Command Line Arguments
==============

1. To infer shape 
			specifications on top of ordering and containment properties for data structure programs, run

		./msolve.py -no_hoflag -reachability [ML source file]


2. To infer specifications for general high-order functional programs, run
       
		./msolve.py -hoflag [ML source file]
			
		
3. To turn off the support for higher-order functions (for first-order programs), run

		./msolve.py -no_hoflag [ML source file]
		
4. To infer quantified array invariants, run

 		./moslve.py -inv -effect [ML source file]
		
