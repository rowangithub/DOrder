DOrder
==================

<a href="https://www.cs.purdue.edu/homes/zhu103/pubs/draft.pdf">
	Link to the paper on Automatically Learning Shape Specifications.</a>
	
<a href="https://www.dropbox.com/s/hrk7nyqbifa25on/PLDI.ova?dl=0">
	Link to the virtual machine of DOrder.</a>	

To play with the virtual machine, please read the section
<a href="#AEC">"PLDI'16 AEC: Run the benchmarks of the paper"</a>.
	
<h3>Overview</h3>
	
DOrder is copyrighted by Purdue University.

Author: He Zhu, Gustavo Petri, Suresh Jagannathan.	
	
<strong>DOrder</strong> is a specification synthesizer written in OCaml that
runs on top of the OCaml compiler. It is capable of synthesizing shape 
specifications for OCaml data structure programs with <strong>no</strong> user-annotations. 
It only requires a <strong>small</strong> number of <strong>simple tests</strong>
to bootstrap synthesis. 

Below, we provide a guide for fun things you can play with DOrder, with
pointers to the <a href="https://www.cs.purdue.edu/homes/zhu103/pubs/draft.pdf">paper</a> 
for further information.

You can also git-clone the source code of DOrder:

		git clone https://github.com/rowangithub/DOrder.git
	
System requirements:

1. OCaml 3.12:

	The tool is currently incompatible with OCaml 4.0+. We hope to improve our code in the future.
	The following instructions assume OCaml library is installed under /usr/local/lib/ocaml/, which is
	also the default setting. Please make necessary changes according to your machine.
	
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



<a name="AEC">PLDI'16 AEC: Run the benchmarks of the paper</a>
===========

This section gives an example about how to validate DOrder. 

<h3>Benchmark location:</h3>
		
		./tests/reachability/
		
To run a benchmark [bench]:

		./msolve.py -no_hoflag -reachability ./tests/reachability/[bench]

<h3>More explanations about benchmarks:</h3>

1. We can infer and verify specifications involving rich ordering properties of data structures 
(e.g. in AVL insertion function, the in-order relation of the output binary tree preserves the in-order relations of the input binary tree;
in list reversal function, the forward-order relation of the output list is equivalent to the backward-order relation of the input list;
in heap merge function, the parent-child relation of the output heap preserves the parent-child relations of the input heaps). 
We support arbitrary user-defined algebra data types. Examples include AVL tree, Splay tree, Braun tree, Skew heap, Treap, etc.

		To try an example, run ./msolve.py -no_hoflag -reachability ./tests/reachability/binarysearchtree.ml
		
2. You should be able to observe specifications inferred for each function in _binarysearchtree_ from your command line interface, 
which will be explained below. If you prefer to observe the output in a file, add "-dump_specs" parameter, you can then read 
synthesized specifications in a file "./specifications.txt".
					
		e.g. ./msolve.py -no_hoflag -reachability -dump_specs ./tests/reachability/binarysearchtree.ml
				

3. We also support the inference and verification of shape-data specifications. For example, we can infer and verify functional
correctness specifications for classic list sorting algorithms (e.g. quicksort, mergesort and heapsort) or 
balanced tree data structure programs (e.g. AVL and Redblack),
proving lists are correctly sorted or trees correctly satisfy BST properties.
Running our tool using the above commands will also display all synthesized shape-data specifications.


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

	let list n = random _n_ integers

	let main () = 
		let _ = fprintf outch "env:newtest\t\n" in
		heapsort (list 15)
	let _ = main ()

The line "let _ = fprintf outch "env:newtest\t\n" in" is used to tell
DOrder to collect input-output behaviors of the function below it 
(e.g. _heapsort_). The function is then called with a randomly generated
list whose length equals 15. 

<h3>Output explanation:</h3>

If "-dump_specs" is used as a parameter to call DOrder, you can
read synthesized specifications in "./specifications.txt". Otherwise,
you can directly read synthesized result from the command line interface.

Synthesized specifications are boolean combinations of a set of atomic 
predicates inferred _per-datatype_. For example, consider the data type 
_heap_ provided in the _heapsort_ program.

	type 'a heap = 
		| E 
		| T of int * 'a * 'a heap * 'a heap
		
A number of atomic predicates are created for this data type,
which essentially is a tree data structure.
We will use _h_ to represent an instance of 'a heap.
Following Section.2 of the paper,
we first consider possible containment predicates for _h_:

		reach (h, u) represents a certain value u is present in a heap h:
		
A more interesting predicate class is one that establishes
ordering relations between two elements of a data structure,
u and v. Recall that in _'a_ _heap_ definition only _T_ constructors
contain values. However, since _T_ contains two
inductively defined subtrees, there are several cases to consider
when establishing an ordering relation among values
found within a tree _h_. We use <strong>link (h, t, i, j, u, v)</strong> 
to represent the ordering relation that u is contained the <strong>i-th</strong> component and 
v is in the <strong>j-th</strong> component of constructor _T_ in _h_ (_T_ is uncapitalized
in the predicate).
For example, if we are interested in cases where the
value u appears “before” (according to a specified order) v,
we could either have that: 

		(i) the value v occurs in the first (left) subtree (indexed by 2) from a tree 
		node containing u (indexed by 1), described by the notation link (h, t, 1, 2, u, v), 

		(ii) the value v occurs in the second (right) subtree (indexed by 3), described by the 
		notation link (h, t, 1, 3, u, v),

		(iii) both values are in the tree, but u is found in a subtree that is disjoint from the subtree 
		where v occurs. Suppose there exists a node whose first subtree contains u (indexed by 2) and whose 
		second subtree contains v (indexed by 3). This is denoted as link (h, t, 2, 3, u, v). 

Notice that in this description we have exhausted all possible relations between any two
values in a tree.		
The first argument (indexed by 0) to the _T_ constructor is not considered by all atomic predicates 
because integer is not part of the polymorphic data structure _'a_ _heap_ (type theory).


<strong><a name="simplification">Simplification:</a></strong> Given a predicate link (h, t, i, j, u, v), 
to improve _readability_ of DOrder, for a polymorphic data structure _'a_ _type_,
if the i-th component of constructor _T_ is the only polymorphic argument of _T_ 
that is not an inductive data type (e.g. simple list or tree data structure), 
we simplify the output of the predicate to link (h, t, j, u, v) because 
i is obvious and hence hidden in this case. 

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
				forall (u v ). ((not link (V, t, 1, 2, u, v)) or 
				link (h2, t, 1, 2, u, v) or
				link (h2, t, 1, 3, u, v) or
				(link (h1, t, 1, 3, u, v) or 
				link (h1, t, 1, 2, u, v)) or
				((reach (h2, u)) and (reach (h1, v))) or
				((reach (h2, v)) and (reach (h1, u)))) /\ ...}

We only show one predicate in the result for simplicity.
In the result type, V represents the value of the result heap. The given specification 
states that the parent-child relation (e.g. link (V, t, 1, 2, u, v) where u and v are quantified) 
between elements contained in the result heap preserves their parent-child relation 
(e.g. link (h2, t, 1, 2, u, v)) in the input heap h1 and h2. [You might find that
link (V, t, 1, 2, u, v) is simplified to link (V, t, 2, u, v) in the result due to the 
<a href="#simplification">simplification strategy</a>.]

DOrder also outputs _shape-data_ specifications. For example, for the _heapsort_ function,
the following specification is synthesized:

	function heapsort with type ls: 'a list ->
        {'a list | forall (u v ). ((not link (V, cons, 0, 1, u, v)) or  (v <= u)) /\ ...}

In the result type, we see that the output list is correctly sorted, where _cons_
is the uncapitalized version the Cons data type constructor of list. The predicate
link (V, cons, 0, 1, u, v) encodes that v is in the tail of a list node containing u.

<h3>How to validate DOrder:</h3>

To validate our experimental results, DOrder displays detailed runtime information in the
command line interface. For example, assume you run DOrder with
	
	>>> ./msolve.py -no_hoflag -reachability -dump_specs ./tests/reachability/heapsort.ml
	
	[ ... ... ]
	
	##time##

	Time to solve constraints:
	TOTAL                         29.109 s
		learn_from_samples             9.160 s
		
	##Size of hypothesis domain: 81##
	##In total 28 specifications were synthesized in the above command lines. QED.
	
Here the number of atomic predicates in the hypothesis domain of all the functions in _heapsort_
is 81 (resp. column H of Tab.5), the number of verified ordering specifications in terms of either 
input-output or shape-data relations is 28 (resp. column I of Tab.5). The total time taken 
(learning and verification) is 29.109s (resp. column T of Tab.5). 
The time spent solely on learning (including the time spent in sampling) is 9.160s 
(resp. column LT of Tab.5).
Inferred specifications can be found either in command lines or "./specifications.txt" depending on
whether "-dump_specs" is used as a parameter to call DOrder.	

Readers are welcome to validate the experimental results listed in Tab.5 of the paper,
following these steps.

<h3>More examples:</h3> 
 
<a href="https://www.cs.purdue.edu/homes/zhu103/pubs/example.pdf">
	More examples on DOrder output (example syntax follows the paper and
	is directly consistent with link and reach predicates given above).</a> 
        

Learning other specifications beyond the paper
===========

3. In addition to the above ordering properties, DOrder can also infer and verify inductive numeric specifications for data structures. For example,
we can infer and verify functional correctness specifications for balanced tree structures (e.g. AVL and Redblack), proving trees can be correctly balanced in
data structure implementations. The corresponding inductive data structure benchmarks are included in ./tests/dml/ directory. 
	
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
		
