# Refinement of Abstraction in Horn clauses using Finite Tree automata

(RAHFT) RAHFT is a tool based on abstraction refinement for verifying
Constrained Horn clauses using abstract interpretation and finite tree
automata.

## Programming 

RAHFT is written in Ciao prolog and is interfaced with Parma polyhedra
libray and Yices SMT solver for handling constraints.  RAHFT uses
several reusable components such as Convex polyhedra analyser,
Query-answer tranformer etc. It also includes a Java library for
manipulating finite tree automata.

## Requirements
1. [Ciao](http://github.com/ciao-lang/ciao) with
   [Parma Polyhedra Library](http://bugseng.com/products/ppl/) support
   (installed with `./ciao-boot.sh local-install
   --contrib:with_ppl=yes --contrib:auto_install_ppl=yes`)
2. Ciao bindings for [Yices SMT solver](http://yices.csl.sri.com/) (`ciao get github.com/jfmc/ciao_yices`)

## Build and installation

You can automatically fetch, build, and install RAHFT using:

    ciao get github.com/bishoksan/RAHFT

Binaries will be placed in the `build/bin` directory relative to the
Ciao workspace directory specified by the `CIAOPATH` environment
variable (hidden `~/.ciao` if undefined).

For developing RAHFT it is recommended to define `CIAOPATH` (E.g.,
`~/ciao`) and clone this repository in your workspace.

To call the `rahft` command without specifying its full path it is
recommended to include in your `PATH` (replace `CIAOPATH` with
`~/.ciao` if undefined):

    export PATH=$CIAOPATH/build/bin:$PATH

## Input and output

**Input**: a set of Horn clauses together with integrity constraints. They
are written using Prolog notation:

e.g. a clause is written as: `h(X):- C, b1(X1),...,bn(Xn).`

and an integrity constriant is written as `false :- C, b1(X1),...,bn(Xn).`

**Output**: safe or unsafe.

**How to run**: `rahft` \<*input file containing a set of Horn clauses*\>

## References

1. Horn clause verification with convex polyhedral abstraction and
   tree automata-based refinement by Bishoksan Kafle and John
   P. Gallagher in COMLAN 2015
   (http://www.sciencedirect.com/science/article/pii/S1477842415000822).

2. Tree Automata-Based Refinement with Application to Horn Clause
   Verification by Bishoksan Kafle and John P. Gallagher in VMCAI 2015
   (http://link.springer.com/chapter/10.1007%2F978-3-662-46081-8_12#page-1)

3. Constraint Specialisation in Horn Clause Verification by Bishoksan
   Kafle and John P. Gallagher in PEPM 2015
   (http://akira.ruc.dk/~kafle/publications/pepm-15)
