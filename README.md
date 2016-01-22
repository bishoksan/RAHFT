# Refinement of Abstraction in Horn clauses using Finite Tree automata (RAHFT)
RAHFT is a  tool based on  abstraction refinement for verifying Constrained Horn clauses using abstract interpretation and finite tree automata. 

## Programming 
RAHFT is written in Ciao prolog  and is interfaced with Parma polyhedra libray and Yices SMT solver for handling constraints.  RAHFT uses several reusable components such as Convex polyhedra analyser, 
Query-answer tranformer etc.. It also includes a Java library for manipulating finite tree automata.

## Requirements
1. Ciao prolog
2. Parma polyhedra libray
3. Yices SMT solver

## Input and output:
Input: a set of Horn clauses together with integrity constraints. They are written using Prolog notation:

e.g. a clause is written as: h(X):- C, b1(X1),...,bn(Xn). 

and an integrity constriant is written as false:- C, b1(X1),...,bn(Xn).

Output: safe or unsafe.

## How to run:
1. ciao src/rahft
2. src/rahft <input file containing a set of Horn clauses> 


## References:
1. Horn clause verification with convex polyhedral abstraction and tree automata-based refinement by Bishoksan Kafle and John P. Gallagher in COMLAN 2015 (http://www.sciencedirect.com/science/article/pii/S1477842415000822). 
2. Tree Automata-Based Refinement with Application to Horn Clause Verification by Bishoksan Kafle and John P. Gallagher in VMCAI 2015 (http://link.springer.com/chapter/10.1007%2F978-3-662-46081-8_12#page-1)
