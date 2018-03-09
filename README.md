# Refinement of Abstraction in Horn clauses using Finite Tree automata

(RAHFT) RAHFT is a tool based on abstraction refinement for verifying
Constrained Horn clauses using abstract interpretation and finite tree
automata. The correctness of results are with respect to the `input Horn clauses` and over the theory
of `linear int arithmetic`.

## Programming 

RAHFT is written in Ciao and is interfaced with Parma polyhedra
library and Yices SMT solver for handling constraints.  RAHFT uses
several reusable components such as Convex polyhedra analyser,
Query-answer tranformer etc. It also includes a Java library for
manipulating finite tree automata.

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

## Build and installation

You can automatically fetch, build, and install RAHFT using:

```sh
ciao get github.com/bishoksan/RAHFT
```

The following dependendencies (including third-party code) will be
installed automatically:

1. [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for
   [Parma Polyhedra Library](https://bugseng.com/products/ppl/)
   (`ciao get ciao_ppl`)
2. [Ciao bindings](https://github.com/jfmc/ciao_yices) for
   [Yices SMT solver](https://yices.csl.sri.com/)
   (`ciao get github.com/jfmc/ciao_yices`)
3. [CHCLibs](https://github.com/bishoksan/chclibs)
   (`ciao get github.com/bishoksan/chclibs`)

All code will be downloaded and built under the first directory
specified in the `CIAOPATH` environment variable or `~/.ciao` by
default.

**For developing** it is recommended to define your own
_workspace directory_ and clone this repository. E.g., `export
CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`.
The dependencies can be cloned manually or fetched automatically by
calling `ciao fetch` at the source directory.

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

4. Rahft: A tool for verifying Horn clauses using abstract interpretation and finite tree automata by Bishoksan Kafle, John P. Gallagher and Jose F. Morales to appear in CAV 2016.
