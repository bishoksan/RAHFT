:- module(atomicProps,[main/1],[]).

:- use_module(library(write)).
:- use_module(library(dynamic)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).

:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).

:- include(chclibs(get_options)).

:- data flag/1. % TODO: use
:- dynamic(fact/2).

go(F,OutPFile) :-
	main(['-prg',F,'-o',OutPFile]).
	
recognised_option('-prg', programO(R),[R]).
recognised_option('-o',   outputFile(R),[R]).

main(ArgV) :-
	cleanup,
	write(user_output, 'Starting ...'), nl(user_output),
	setOptions(ArgV,File,OutS),
	load_file(File),
	add_false_clauses,
	%start_time,
	start_ppl,
	operator,
	%end_time(user_output),
	showallfacts(OutS),
	nl(OutS),
	close(OutS),
	ppl_finalize.

setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	( member(programO(File),Options)
	; write(user_output,'No input file given.'),nl(user_output)
	),
	( member(outputFile(user_output),Options) ->
	    OutS=user_output
	; member(outputFile(OutFile),Options), open(OutFile,write,OutS)
	; OutS=user_output
	).

cleanup :-
	retractall(fact(_,_)),
	retractall(my_clause(_,_,_)).
	
add_false_clauses :-
	( my_clause(false,B,_) ; my_clause(false_ans,B,_) ),
	separate_constraints(B,Cs,Bs),
	negAtoms(Cs,Ns),
	assert_false_clauses(Bs,Cs),
	assert_false_neg_clauses(Bs,Ns),
	fail.
add_false_clauses.
	
assert_false_clauses([],_).
assert_false_clauses([B|Bs],Cs) :-
	assertz(my_clause(B,Cs,x)),
	assert_false_clauses(Bs,Cs).
	
negAtoms([X=<Y|Cs],[X>Y|Ns]) :-
	negAtoms(Cs,Ns).
negAtoms([X>=Y|Cs],[X<Y|Ns]) :-
	negAtoms(Cs,Ns).
negAtoms([X<Y|Cs],[X>=Y|Ns]) :-
	negAtoms(Cs,Ns).
negAtoms([X>Y|Cs],[X=<Y|Ns]) :-
	negAtoms(Cs,Ns).
negAtoms([X=Y|Cs],[X<Y,X>Y|Ns]) :-
	negAtoms(Cs,Ns).
negAtoms([],[]).

assert_false_neg_clauses([],_).
assert_false_neg_clauses([B|Bs],Cs) :-
	assert_each_atom_clause(Cs,B),
	assert_false_neg_clauses(Bs,Cs).
	
assert_each_atom_clause([],_).
assert_each_atom_clause([C|Cs],B) :-
	assertz(my_clause(B,[C],x)),
	assert_each_atom_clause(Cs,B).
	
operator:-
	my_clause(Head,B,_),
	separate_constraints(B,Cs,_),
	Head =.. [_|Xs],
	solve(Xs,Cs,Hp),
	record(Head,Hp),
	fail.
operator.
	
solve(Xs,Cs,Hp) :-
	linearize(Cs,Cs1),
	varset((Xs,Cs1),Ys),
	dummyCList(Ys,DCL),
	append(Cs1,DCL,CsL),
	numbervars((Xs:-CsL),0,_),
	satisfiable(CsL,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp).

record(Head,H):-
	cond_assert(Head,H).

cond_assert(Head,H):-
	\+ alreadyAsserted(Head,H),
	assertz(fact(Head,H)).

alreadyAsserted(Head,H) :-
	fact(Head,H1), 
	entails(H,H1),
	entails(H1,H).

showallfacts(S) :-
	fact(F,H),
	getConstraint(H,C),
	writeq(S,F), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	fail.
showallfacts(_).



