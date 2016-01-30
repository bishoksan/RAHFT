:- module(atomicProps,_).

:- use_module(setops).
:- use_module(canonical).
:- use_module(linearize).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(timer_ciao).
:- use_module(input_ppl_clausenum).
:- use_module(ppl_ops).

:- include(common).

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
	( member(outputFile(user_outout),Options) ->
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
	assert(my_clause(B,Cs,x)),
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
	assert(my_clause(B,[C],x)),
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
	
dummyCList([],[]).
dummyCList([C|Cs],[C=C|Cs1]) :-
	dummyCList(Cs,Cs1).

record(Head,H):-
	cond_assert(Head,H).

cond_assert(Head,H):-
	\+ alreadyAsserted(Head,H),
	assert(fact(Head,H)).

alreadyAsserted(Head,H) :-
	fact(Head,H1), 
	entails(H,H1),
	entails(H1,H).

separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).

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



