:- module(insertProps, [main/1], []).

:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(dynamic)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).

:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(input_ppl_clausenum)).
:- use_module(chclibs(ppl_ops)).

:- include(chclibs(common)).

:- dynamic(fact/2).
:- dynamic(prop/2).
:- dynamic(pe_clause/2).

recognised_option('-prg',  programO(R),[R]).
recognised_option('-props',  propFile(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

main(ArgV) :-
	cleanup,
	write(user_output,'Starting ...'),
	nl(user_output),
	setOptions(ArgV,File,OutS),
	load_file(File),
	%start_time,
	start_ppl,
	operator,
	%end_time(user_output),
	writeClauses(OutS),
	nl(OutS),
	close(OutS),
	ppl_finalize.

	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options) -> true; 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
				OutS=user_output),
	(member(propFile(PFile),Options), readPropFile(PFile); 
			true).

cleanup :-
	retractall(fact(_,_)),
	retractall(prop(_,_)),
	retractall(my_clause(_,_,_)),
	retractall(pe_clause(_,_)).
	
operator:-
	my_clause(Head,B,_),
	separate_constraints(B,Cs,Bs),
	answerConstraint(Head,Cs1),
	%findall(prop(Head,Cs1),prop(Head,Cs1),Props),
	%Props = [_,_], % exists both an answer and a query 
	%appendConstraints(Props,Head,Cs,Cs2),
	append(Cs1,Cs,Cs2),
	bodyAnswerConstraints(Bs,Cs3),
	append(Cs2,Cs3,Cs4),
	numbervars((Head,Cs4,Bs),0,_),
	satisfiable(Cs4,H),
	getConstraint(H,Cs5),
	append(Cs5,Bs,B1),
	assertz(pe_clause(Head,B1)),
	fail.
operator.

bodyAnswerConstraints([],[]).
bodyAnswerConstraints([B|Bs],Cs) :-
	bodyAnswerConstraints(Bs,Cs1),
	answerConstraint(B,Cs2),
	append(Cs2,Cs1,Cs).
	
answerConstraint(A,Cs) :-
	A =.. [P|Xs],
	name(P,PName),
	append(PName,"_ans",AName),  % first look for the _ans predicate
	name(P_ans,AName),
	A_ans =.. [P_ans|Xs],
	prop(A_ans,Cs),
	!.
answerConstraint(A,Cs) :-        % otherwise just get the original predicate
	prop(A,Cs).
	
appendConstraints([],_,Cs,Cs).
appendConstraints([prop(Head,Cs1)|Props],Head,Cs,Cs3) :-
	appendConstraints(Props,Head,Cs,Cs2),
	append(Cs1,Cs2,Cs3).
	
unsat(Cs) :-
	linearize(Cs,Cs1),
	numbervars(Cs1,0,_),
	\+ satisfiable(Cs1,_).
	
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
	

readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	varset(H,Xs),
	dummyCList(Xs,DCL),
	append(C,DCL,CsL),
	assertz(prop(H,CsL)),
	read(S,C1),
	readPropFacts(S,C1).

writeClauses(S) :-
	pe_clause(H,B),
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	writeBodyAtoms(S,B),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_).
	
writeBodyAtoms(S,[]) :-
	!,
	write(S,'   '),
	write(S,true).
writeBodyAtoms(S,[B]) :-
	!,
	write(S,'   '),
	writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
	write(S,'   '),
	writeq(S,B1),
	write(S,','),
	nl(S),
	writeBodyAtoms(S,[B2|Bs]).