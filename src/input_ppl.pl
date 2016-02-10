:- module(input_ppl, [load_file/2,my_clause/2], []).

:- use_module(library(lists)).
:- use_module(library(read)).
:- use_module(library(dec10_io)).
:- use_module(library(dynamic)).
:- use_module(duplVar).

:- dynamic my_clause/2.

:- dynamic inputtype/1.

load_file(F,Type) :-
    retractall(my_clause(_,_)),
    assertz(inputtype(Type)),
	see(F),
	remember_all,
	seen.

remember_all :-
	read(C),
	(
	    C == end_of_file -> true
	;
	    remember_clause(C),
	    remember_all
	).

remember_clause((A :- B)) :-
%only execute+specialize clauses are needed
    (inputtype(pic) -> true, keepclauses(A)
    ;
    inputtype(pl) -> true),
	!,
    atomconstraints(A, ACs0,ACs1, Ant),
	writeAtomEq(Ant,Anodupl,Es0,Es1),
	
	tuple2list(B,BL),
	bodyconstraints(BL,BL0,BCs0,BCs1),
	ACs1=Es0,
	Es1=BCs0,
	BCs1=BL0,
	assertz(my_clause(Anodupl,ACs0)).

remember_clause(A) :-
   (inputtype(pic) -> true, keepclauses(A)
    ;
    inputtype(pl) -> true),
    !,
    atomconstraints(A, ACs0, ACs1, Ant),
	writeAtomEq(Ant,Anodupl,Es0,[]),
	ACs1=Es0,
	assertz(my_clause(Anodupl,ACs0)).

%Drop all non-execute/specialize clauses
remember_clause(_).



keepclauses(A) :-
    A =.. [F|_],
    name(F,Fn),
    name(execute,En),
    conc(En,_,Fn).
keepclauses(A) :-
    A =.. [F|_],
    name(F,Fn),
    name(specialize,En),
    conc(En,_,Fn).

conc([],L,L).
conc([A|L1],L2,[A|L3]) :-
     conc(L1,L2,L3).

atomconstraints(H,Cs0,Cs1,H1) :-
  H =.. [P|Xs],
  genConstraints(Xs,Ys,Cs0,Cs1),
  H1 =.. [P|Ys].

genConstraints([],[],Cs,Cs).
genConstraints([X|Xs],[Y|Ys],[X=Y|Cs0],Cs1) :-
     var(X),
     occurs(X,Xs),
     !,
     genConstraints(Xs,Ys,Cs0,Cs1).
genConstraints([X|Xs],[X|Ys],Cs0,Cs1) :-
     var(X),
     !,
     genConstraints(Xs,Ys,Cs0,Cs1).
genConstraints([T|Xs],[Y|Ys],[Y=T|Cs0],Cs1):-
    genConstraints(Xs,Ys,Cs0,Cs1).


occurs(X,[Y|_]) :-
   X == Y,
   !.

occurs(X,[_|Ys]) :-
   occurs(X,Ys).



tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

bodyconstraints([],[],Cs,Cs).
bodyconstraints([B|Bs],[B|Bs1],Cs0,Cs1) :-
	constraint(B,_),
	!,
	bodyconstraints(Bs,Bs1,Cs0,Cs1).
bodyconstraints([B|Bs],[B2|Bs1],Cs0,Cs1) :-
	atomconstraints(B,Cs0,BCs1,B1),
	writeAtomEq(B1,B2,Es0,Es1),
	BCs1=Es0,
	bodyconstraints(Bs,Bs1,Es1,Cs1).
	
constraint(X=Y, X=Y).
constraint(X=:=Y, X=Y).
constraint(X is Y, X = Y).
constraint(X>Y, X>Y).
constraint(X>=Y, X>=Y).
constraint(X=<Y, X=<Y).
constraint(X<Y, X<Y).
