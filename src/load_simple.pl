:- module(load_simple, [load_file/1,my_clause/3], []).

:- dynamic my_clause/3.

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(read)).

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
	tuple2list(B,BL),
	makeClauseId(K,CK),
	assertz(my_clause(A,BL,CK)).

remember_clause(A,K) :-
	makeClauseId(K,CK),
	assertz(my_clause(A,[],CK)),
	!.
%Drop all non-execute/specialize clauses
remember_clause((:- _),_).

makeClauseId(K,CK) :-
	name(K,NK),
	append("c",NK,CNK),
	name(CK,CNK).



tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

