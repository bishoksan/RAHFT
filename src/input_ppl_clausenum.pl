:- module(input_ppl_clausenum,[load_file/1,my_clause/3],[]).

:- dynamic my_clause/3.

:- use_module(library(dynamic)).
:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(duplVar).

load_file(F) :-
    retractall(my_clause(_,_,_)),
	open(F,read,S),
	remember_all(S,1),
	close(S).

remember_all(S,K) :-
	read(S,C),
	(
	    C == end_of_file -> true
	;
	    remember_clause(C,K),
	    K1 is K+1,
	    remember_all(S,K1)
	).

remember_clause((A :- B),K) :-
	!,
    atomconstraints(A, ACs0,ACs1, Ant),
	writeAtomEq(Ant,Anodupl,Es0,Es1),
	
	tuple2list(B,BL),
	bodyconstraints(BL,BL0,BCs0,BCs1),
	ACs1=Es0,
	Es1=BCs0,
	BCs1=BL0,
	makeClauseId(K,CK),
	assertz(my_clause(Anodupl,ACs0,CK)).

remember_clause(A,K) :-
    atomconstraints(A, ACs0, ACs1, Ant),
	writeAtomEq(Ant,Anodupl,Es0,[]),
	ACs1=Es0,
	makeClauseId(K,CK),
	assertz(my_clause(Anodupl,ACs0,CK)),
	!.
%Drop all non-execute/specialize clauses
remember_clause(_,_).

makeClauseId(K,CK) :-
	name(K,NK),
	append("c",NK,CNK),
	name(CK,CNK).




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
