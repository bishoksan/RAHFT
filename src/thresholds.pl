:- module(thresholds,_).

:- use_module(setops).
:- use_module(canonical).
:- use_module(linearize).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(timer_ciao).
:- use_module(input_ppl_clausenum).
:- use_module(ppl_ops).

:- dynamic(fact/2).
:- dynamic(prop/2).

	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	%start_time,
	start_ppl,
	
	write('1st iteration'),
	nl,
	% 1st iteration
	initProps,
	operator,
	%showallfacts(user_output), nl(user_output),
	clearprops,
	facts2props,
	%atomicprops,
	clearfacts,
	
	write('2nd iteration'),
	nl,
	% 2nd iteration
	operator,
	%showallfacts(user_output), nl(user_output),
	clearprops,
	%atomicprops,
	facts2props,

	clearfacts,

	write('3rd iteration'),
	nl,
	% 3rd iteration
	operator,
	%showallfacts(user_output), nl(user_output),
	clearprops,
	write('Making atomic props'),
	nl,
	atomicprops,
	
	%end_time(user_output),
	%showallfacts(OutS), nl(OutS),
	write('Writing out threshold facts'),
	nl,
	showallprops(OutS),
	nl(OutS),
	close(OutS),
	ppl_finalize.
	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output)),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).

% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   get_options(RT,OT,AT).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

cleanup :-
	retractall(fact(_,_)),
	retractall(prop(_,_)),
	retractall(my_clause(_,_,_)).

clearprops :-
	retractall(prop(_,_)).

clearfacts :-
	retractall(fact(_,_)).
	
initProps :-
	preds(Ps),
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
	assert_each_atom_prop(Cs,A),
	fail.
atomicprops.

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
	equivalent(H,H1).


prove(Bs,Ds) :-
	prove_any(Bs,Ds).
	
separate_constraints([],[],[]).
separate_constraints([B|Bs],[C|Cs],Ds) :-
	constraint(B,C),
	!,
	separate_constraints(Bs,Cs,Ds).
separate_constraints([B|Bs],Cs,[B|Ds]) :-
	separate_constraints(Bs,Cs,Ds).


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

