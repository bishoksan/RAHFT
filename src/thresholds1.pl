:- module(thresholds1, [main/1], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates)).
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
:- dynamic(prop/2).
:- dynamic(abstract/0).


recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-a',    abstract,[]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	preds(Ps),
	start_ppl,
	initProps(Ps),
	thresholds(3,Ps),
	atomicprops,
	%facts2props,
	%write('Writing out threshold facts'),
	nl,
	showallprops(OutS),
	nl(OutS),
	close(OutS),
	ppl_finalize.
	
thresholds(N,Ps) :-
	N > 0,
	%write('Iteration '), write(N), nl,
	%atomicprops,
	facts2props,
	clearfacts,
	operator,
	%showallfacts(user_output), nl(user_output),
	clearprops,
	(abstract -> abstract(Ps); true),
	%write('Abstract facts'),nl,
	%showallfacts(user_output), nl(user_output),
	N1 is N-1,
	!,
	thresholds(N1,Ps).
thresholds(0,_).

setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output)),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output),
	(member(abstract,Options), assert(abstract); 
			true).



cleanup :-
	retractall(fact(_,_)),
	retractall(prop(_,_)),
	retractall(abstract),
	retractall(my_clause(_,_,_)).

clearprops :-
	retractall(prop(_,_)).

clearfacts :-
	retractall(fact(_,_)).
	
initProps(Ps) :-
	assert_top_values(Ps).
	
preds(Ps) :-
	setof(P/N, [A,B,C]^(my_clause(A,B,C), functor(A,P,N)),Ps),
	!.
preds([]).
	
assert_top_values([]).
assert_top_values([P/N|Ps]) :-
	functor(A,P,N),
	assert(prop(A,[])),
	assert_top_values(Ps).
	
atomicprops :-
	fact(A,H),
	getConstraint(H,Cs),
	A =.. [_|Xs],
	projectVars(Xs,[],H,Cs1),
	append(Cs,Cs1,Cs2),
	assert_each_atom_prop(Cs2,A),
	fail.
atomicprops.

projectVars([],_,_,[]).
projectVars([X|Xs],Ys,H,Cs) :-
	append(Xs,Ys,Zs),
	copyPolyhedron(H,H1),
	project(H1,Zs,H2),
	mapCoords(H2,['$VAR'(0)-X]),
	getConstraint(H2,Cs2),
	projectVars(Xs,[X|Ys],H,Cs1),
	append(Cs2,Cs1,Cs).
	
	
facts2props :-
	fact(A,H),
	getConstraint(H,Cs),
	melt(prop(A,Cs),Prop),
	assert(Prop),
	fail.
facts2props.
	
assert_each_atom_prop([],B) :-
	!,
	checkAssert(prop(B,[])).
assert_each_atom_prop([C],B) :-
	!,
	checkAssert(prop(B,[C])).
assert_each_atom_prop([C|Cs],B) :-
	checkAssert(prop(B,[C])),
	assert_each_atom_prop(Cs,B).

checkAssert(P) :-
	existingProp(P),
	!.
checkAssert(P) :-
	melt(P, Prop),
	assert(Prop).
	
existingProp(prop(B,C)) :-
	prop(B,C).
	
operator:-
	my_clause(Head,B,_),
	separate_constraints(B,Cs,Bs),
	Head =.. [_|Xs],
	solve(Bs,Xs,Cs,Hp),
	record(Head,Hp),
	fail.
operator.

abstract([]).
abstract([P/N|Ps]) :-
	%write(P/N),nl,
	functor(A,P,N),
	findall(H,fact(A,H),[H1|Hs]),
	!,
	convexhull([H1|Hs],H2),
	retractall(fact(A,_)),
	numbervars(A,0,_),
	assert(fact(A,H2)),
	abstract(Ps).
abstract([_|Ps]) :-
	abstract(Ps).
	
convexhull([H],H) :-
	!.
convexhull([H|Hs],H1) :-
	convexhull(Hs,H1),
	%getConstraint(H1,C1),
	%getConstraint(H,C),
	%write(C1),write(' -- '),write(C),nl,
	ppl_Polyhedron_poly_hull_assign(H1,H).
	    
solve(Bs,Xs,Cs,Hp) :-
	prove(Bs,Ds),
	linearize(Cs,CsLin),
	append(CsLin,Ds,CsDs),
	varset((Xs,CsDs),Ys),
	dummyCList(Ys,DCL),
	append(CsDs,DCL,CsL),
	numbervars((Xs:-CsL),0,_),
	satisfiable(CsL,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp).
	
	   
record(Head,H):-
	cond_assert(Head,H).
	
cond_assert(Head,H):-
	\+ alreadyAsserted(Head,H),
	assert(fact(Head,H)).
		
alreadyAsserted(Head,H) :-
	fact(Head,H1), 
	equivalent(H,H1).


prove(Bs,Ds) :-
	prove_any(Bs,Ds).

prove_any([],[]).
prove_any([true],[]).
prove_any([B|Bs],Ds) :-
	getanyfact(B,Cs),
	prove_any(Bs,Ds1),
	append(Cs,Ds1,Ds).
	
getanyfact(B,Cs) :-
	prop(B,Cs).

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

showallprops(S) :-
	prop(F,C),
	numbervars((F,C),0,_),
	writeq(S,F), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	fail.
showallprops(_).

