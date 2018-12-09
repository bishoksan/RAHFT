:- module(splitVersions,[main/1],[dynamic]).

% Explicit splitting of predicates based on disjoint set of constraints
% in their bodies. This is motivated by the fact that the convex hull
% of these constraints will give TRUE losing precision for the head
% predicate.
% 
%   Example:
%   
%   q(X):- p(X). 
%   p(X):- X<0, B1.
%   p(X):- X>0, B2.
%   
%   Since for predicate p, the body constraints are disjoint then we split them. 
%   q(X):- p1(X).
%   q(X):- p2(X).
%   
%   p1(X):- X<0, B1.
%   p2(X):- X>0, B2.
%   
%   But we do not split in the case 
%   
%   q(X):- p(X). 
%   p(X):- X<0, B1.
%   p(X):- X=<0, B2.

:- use_module(chclibs(setops)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(aggregates)).

:- dynamic(factc/3).

go2(F,OutPFile) :-
	main(['-prg',F,'-o',OutPFile]).

go(F) :-
	main(['-prg',F]).
	
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	%start_time,
	start_ppl,
	operator,
	predicates(Ps),
	split(Ps,Defs),
	unfold(Defs,Defs,OutS),
	%end_time(user_output),
	nl(OutS),
	close(OutS),
	ppl_finalize.

	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output)),
	(member(outputFile(user_outout),Options) -> OutS=user_output;
			member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
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
	%\+ alreadyAsserted(Head,H,C),
	assert(factc(Head,H,C)).
		
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
	makeDefs(Gs,PDefs,1).
	
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

makeDefs([G|Gs],[splitGroup(PK/N,Cls)|GCls],K) :-
	groupPredName(G,K,PK,N),
	groupDef(G,Cls),
	K1 is K+1,
	makeDefs(Gs,GCls,K1).
makeDefs([],[],_).

groupPredName([factc(H,_,_)|_],K,PK,N) :-
	functor(H,P,N),
	name(P,PName),
	name(K,KName),
	append(PName,[95,95,95|KName],PKName),
	name(PK,PKName).

groupDef([factc(_,_,C)|G],[C|Cs]) :-
	groupDef(G,Cs).
groupDef([],[]).

unfold([proc(_,PDefs)|Defs],AllDefs,S) :-
	makeClauses(PDefs,AllDefs,S),
	unfold(Defs,AllDefs,S).
unfold([],_,_).

makeClauses([splitGroup(PK/_,Cls)|GCls],AllDefs,S) :-
	writeAllClauses(Cls,PK,AllDefs,S),
	nl(S),
	makeClauses(GCls,AllDefs,S).
makeClauses([],_,_).

writeAllClauses([C|Cs],PK,AllDefs,S) :-
	my_clause(Head,Body,C),
	numbervars((Head,Body),0,_),
	renameAtom(Head,PK,HeadK),
	unfoldBody(Body,AllDefs,Bodies),
	writeKClauses(HeadK,Bodies,S),
	writeAllClauses(Cs,PK,AllDefs,S).
writeAllClauses([],_,_,_).

renameAtom(false,_,false) :-
	!.
renameAtom(false_ans,_,false_ans) :-
	!.
renameAtom(false_ans_ans,_,false_ans_ans) :-
	!.
renameAtom(A,PK,AK) :-
	A =.. [_|Xs],
	AK =.. [PK|Xs].

unfoldBody(Body,AllDefs,Bodies) :-
	findall(UBody,unfolded(Body,AllDefs,UBody),Bodies).
	
unfolded([B|Body],AllDefs,[B|UBody]) :-
	constraint(B,_),
	!,
	unfolded(Body,AllDefs,UBody).
unfolded([B|Body],AllDefs,[B1|UBody]) :-
	functor(B,P,N),
	findGroups(AllDefs,P/N,PDefs),
	member(splitGroup(PK/N,_),PDefs),
	renameAtom(B,PK,B1),
	unfolded(Body,AllDefs,UBody).
unfolded([],_,[]).

findGroups([proc(P/N,PDefs)|_],P/N,PDefs) :-
	!.
findGroups([_|AllDefs],P/N,PDefs) :-
	findGroups(AllDefs,P/N,PDefs).
	
writeKClauses(HK,[Body|Bodies],S) :-
	writeq(S,HK),
	write(S,' :-'),
	nl(S),
	writeBodyAtoms(S,Body),
	write(S,'.'),
	nl(S),
	writeKClauses(HK,Bodies,S).
writeKClauses(_,[],_).
	
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

