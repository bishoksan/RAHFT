# Refinement of Abstraction in Horn clauses using Finite Tree automata

(RAHFT) RAHFT is a tool based on abstraction refinement for verifying
Constrained Horn clauses using abstract interpretation and finite tree
automata. The correctness of results are with respect to the `input Horn clauses` and over the theory
of `linear real arithmetic`.

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
2. Ciao bindings for [Yices SMT solver](http://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)

## Build and installation

You can automatically fetch, build, and install RAHFT using:

```sh
ciao get github.com/bishoksan/RAHFT
```

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call `rahft` without specifying its full path
it is recommended to include this directory in your `PATH`:

```sh
export PATH=$CIAOPATH/build/bin:$PATH
# or export PATH=~/.ciao/build/bin:$PATH
```

**For developing** RAHFT it is recommended to define `CIAOPATH` (E.g.,
`~/ciao`) and clone this repository in your workspace.

## Usage

**Usage**: `rahft` \<*input file containing a set of Horn clauses*\>

**Input**: a set of Horn clauses together with integrity constraints. They
are written using Prolog notation:

e.g. a clause is written as: `h(X):- C, b1(X1),...,bn(Xn).`

and an integrity constriant is written as `false :- C, b1(X1),...,bn(Xn).`

**Output**: `safe` or `unsafe`.
The results of the analysis will be appended to the `result.txt` file in the current directory and also shown in the console.

## Generate a standalone binary distribution

```sh
mkdir dist; cd dist
ciaoc_sdyn ../src/rahft
cp ../src/determinise.jar
```

This creates a platform specific binary `rahft` at `dist/`
directory, together with the collection of shared libraries for the
dependencies.

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
