:- module(splitClauseIds, [main/1], []).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(dynamic)).
:- use_module(library(aggregates)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).

:- use_module(chclibs(setops)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).

:- include(chclibs(get_options)).

:- data flag/1. % TODO: use
:- dynamic(factc/3).

go2(F,OutPFile) :-
	main(['-prg',F,'-o',OutPFile]).

go(F) :-
	main(['-prg',F]).
	
recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	start_ppl,
	operator,
	predicates(Ps),
	split(Ps,Defs),
	writeGroups(Defs,OutS),
	nl(OutS),
	close(OutS),
	ppl_finalize.

	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	( member(programO(File),Options)
	; write(user_output,'No input file given.'),nl(user_output)
	),
	( member(outputFile(user_output),Options) -> OutS=user_output
	; member(outputFile(OutFile),Options), open(OutFile,write,OutS)
	; OutS=user_output
	).

cleanup :-
	retractall(factc(_,_,_)),
	retractall(my_clause(_,_,_)).

operator:-
	my_clause(Head,B,C),
	separate_constraints(B,Cs,_),
	Head =.. [_|Xs],
	solve(Xs,Cs,Hp),
	record(Head,Hp,C),
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

	   
record(Head,H,C):-
	cond_assert(Head,H,C).
	
cond_assert(Head,H,C):-
	assertz(factc(Head,H,C)).
		
alreadyAsserted(Head,H) :-
	factc(Head,H1,_), 
	contains(H,H1),
	contains(H1,H).
	


predicates(Ps) :-
	setof(P/N,
		[Head,H,C]^(factc(Head,H,C),functor(Head,P,N))
		,Ps),
	!.
predicates([]).

split([P/N|Ps],[proc(P/N,PDefs)|Defs]) :-
	splitProc(P/N,PDefs),
	split(Ps,Defs).
split([],[]).

splitProc(P/N,PDefs) :-
	findall(factc(Head,H,C),
		(functor(Head,P,N),
		 factc(Head,H,C)),
		Fs),
	disjointGroups(Fs,[],Gs),
	makeDefs(Gs,PDefs).
	
disjointGroups([],Gs,Gs).
disjointGroups([F|Fs],Gs0,Gs2) :-
	insertGroup(F,Gs0,Gs1),
	disjointGroups(Fs,Gs1,Gs2).
		
insertGroup(F,[],[[F]]).
insertGroup(F,[G|Gs],[G|Gs1]) :-
	disjointFromGroup(F,G),
	!,
	insertGroup(F,Gs,Gs1).
insertGroup(F,[G|Gs],[G1|Gs]) :-
	append(G,[F],G1).

disjointFromGroup(factc(Head,H,C),[factc(Head,H1,_)|G]) :-
	disjointFrom(H,H1),
	disjointFromGroup(factc(Head,H,C),G).
disjointFromGroup(_,[]).

makeDefs([G|Gs],[Cls|GCls]) :-
	groupDef(G,Cls),
	makeDefs(Gs,GCls).
makeDefs([],[]).

groupDef([factc(_,_,C)|G],[C|Cs]) :-
	groupDef(G,Cs).
groupDef([],[]).

writeGroups([G|Gs],S) :-
	writeq(S,G),
	write(S,'.'),
	nl(S),
	writeGroups(Gs,S).
writeGroups([],_).
	



	

