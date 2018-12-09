:- module(ftaRefine, [main/1], [dynamic]).

% Input: Program (P), a determined finite tree autoamta (DFTA),
%   optionally a file containing a list of states in DFTA to be splitted
%   (SF)
% 
% Output: Program (P1).
% 
% If SF is present, the procedure splits the states in DFTA using the
% information in SF producing a new automata D1 otherwise D1=D
% If D1 contains a state containing "errortrace" that state is
% eliminated from D1.
% Then it produces P1 from P and D1 (see Algorithm 2 of
% http://akira.ruc.dk/~kafle/publications/comlan-15).

:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(lists)).

:- use_module(chclibs(builtins)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).

:- include(chclibs(get_options)).

:- dynamic transition/3.
:- dynamic new_clause/2.
:- dynamic statePred/3.
:- dynamic nameCounter/1.
:- dynamic split/1.

recognised_option('-prg',  program(R),[R]).
recognised_option('-fta',  ftaFile(R),[R]).
recognised_option('-split',  splitFile(R),[R]).
recognised_option('-o',    outputFile(R),[R]).


main(ArgV) :-
	cleanup,
	get_options(ArgV,Options,_),
	setOptions(Options,File,FTA,OutS),
	load_file(File),
	load_fta(FTA,Trs),
	splitStates(Trs,Trs1),
	makeClauses(Trs1),
	makeExtraClauses,
	writeClauses(OutS),
	close(OutS).

:- data flag/1.

setOptions(Options,File,FTA,OutS) :-
	( member(program(File),Options) -> true
	; write('No input file given.'), nl,
	  fail
	),
	( member(ftaFile(FTA),Options) -> true 
	; write('No FTA file given.'), nl,
	  fail
	),
	( member(splitFile(Split),Options) ->
	    assertz(split(Split))
	; assertz(split('$NOSPLIT'))
	),
	( member(outputFile(OutFile),Options) ->
	    open(OutFile,write,OutS)
	; OutS=user_output
	).
			
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(new_clause(_,_)),
	retractall(transition(_,_,_)),
	retractall(nameCounter(_)),
	retractall(statePred(_,_,_)),
	retractall(split(_)),
	assertz(nameCounter(0)).

makeClauses([transition(Left,Right,C)|Trs]) :-
	my_clause(H,B,C),
	separate_constraints(B,Cs,Bs),
	renameHead(H,Right,H1),
	renameBody(Bs,Left,Bs1),
	append(Cs,Bs1,Bs2),
	assertz(new_clause(H1,Bs2)),
	makeClauses(Trs).
makeClauses([]).

makeExtraClauses :-
	statePred(Qs,N,Q),
	productState(Qs),
	epsilonClauses(Qs,N,Q),
	fail.
makeExtraClauses.

splitStates(Trs,Trs) :-
	split('$NOSPLIT'),
	!.
splitStates(Trs,Ts) :-
	split(SplitFile),
	open(SplitFile,read,S),
	readSplits(S,ProcSplits),
	errorSharePreds(ProcSplits,Es),
	splitTransitions(Trs,Es,Ts,[]).
	
splitTransitions([T|Trs],Es,Ts0,Ts2) :-
	containsSplitPred(T,Es,_,_),
	!,
	splitTrans([T],Es,Ts0,Ts1),
	splitTransitions(Trs,Es,Ts1,Ts2).
splitTransitions([T|Trs],Es,[T|Ts0],Ts1) :-
	splitTransitions(Trs,Es,Ts0,Ts1).
splitTransitions([],_,Ts,Ts).

containsSplitPred(transition(Ys,_,_),Es,P/N,J) :-
	nth(J,Ys,state([[P]],P/N)),
	splittable(Es,P/N),
	!.
containsSplitPred(transition(Ys,_,_),Es,P/N,J) :-
	nth(J,Ys,state(Qs,P/N)),
	member([P],Qs),
	splittable(Es,P/N),
	!.
containsSplitPred(transition(_,state([P],P/N),_),Es,P/N,0) :-
	splittable(Es,P/N),
	!.
	
splittable([proc(P/N,[_,_|_])|_],P/N) :-
	!.
splittable([_|Es],P/N) :-
	splittable(Es,P/N).
	
splitTrans([T|Ts],Es,Ts0,Ts1) :-
	containsSplitPred(T,Es,P/N,J),
	J > 0,
	!,
	member(proc(P/N,Groups),Es),
	replaceState(Groups,T,1,J,T1s,Ts),
	splitTrans(T1s,Es,Ts0,Ts1).
splitTrans([T|Ts],Es,[T1|Ts0],Ts1) :-
	containsSplitPred(T,Es,P/N,0),
	!,
	member(proc(P/N,Groups),Es),
	splitHead(T,T1,Groups),
	splitTrans(Ts,Es,Ts0,Ts1).
splitTrans([T|Ts],Es,[T|Ts0],Ts1) :-
	splitTrans(Ts,Es,Ts0,Ts1).
splitTrans([],_,Ts,Ts).

splitHead(transition(Qs,state([P],P/N),C),transition(Qs,state([PVJ],P/N),C),Groups) :-
	nth(J,Groups,Grp),
	member(C,Grp),
	name(J,JN),
	append("v",JN,VJN),
	name(VJ,VJN),
	newPredName(P,VJ,PVJ). % P is in the Jth group, so renamed P_vJ
	
replaceState([_|Gs],transition(Qs,state(Q,P/N),C),K,J,[transition(Qs1,state(Q,P/N),C)|Ts1],Ts) :-
	nth(J,Qs,state([[S]],S/M)),
	!,
	name(K,KN),
	append("v",KN,VKN),
	name(VK,VKN),
	newPredName(S,VK,SVK),
	replaceJth(Qs,J,Qs1,state([[SVK]],S/M)),
	K1 is K+1,
	replaceState(Gs,transition(Qs,state(Q,P/N),C),K1,J,Ts1,Ts).
replaceState([_|Gs],transition(Qs,state(Q,P/N),C),K,J,[transition(Qs1,state(Q,P/N),C)|Ts1],Ts) :-
	nth(J,Qs,state(Qk,S/M)),
	nth(I,Qk,[S]),
	name(K,KN),
	append("v",KN,VKN),
	name(VK,VKN),
	newPredName(S,VK,SVK),
	replaceJth(Qk,I,Qk1,[SVK]),
	replaceJth(Qs,J,Qs1,state(Qk1,S/M)),
	K1 is K+1,
	replaceState(Gs,transition(Qs,state(Q,P/N),C),K1,J,Ts1,Ts).
replaceState([],_,_,_,Ts,Ts).
	
replaceJth([_|Qs],1,[P|Qs],P).
replaceJth([Q|Qs],J,[Q|Qs1],P) :-
	J > 1,
	J1 is J-1,
	replaceJth(Qs,J1,Qs1,P).
	
productState([[_|_]|_]).

epsilonClauses([],_,_).
epsilonClauses([Q|Qs],N,P) :-
	statePred(Q,N,Q1),
	functor(Head,P,N),
	Head =.. [P|Xs],
	Body =.. [Q1|Xs],
	assertz(new_clause(Head,[Body])),
	epsilonClauses(Qs,N,P).

renameHead(H,state(Right,P/N),H1) :-
	getStateName(Right,N,P,Q),
	H =..[_|Xs],
	H1=..[Q|Xs].
	
getStateName(Right,N,_,Q) :-
	statePred(Right,N,Q),  	% check for existing name
	!.
getStateName([Q],N,_,Q) :-
	atom(Q),
	!,
	assertz(statePred([Q],N,Q)).
getStateName(Qs,N,P,PK) :-
	getCounter(K),
	newPredName(P,K,PK),
	assertz(statePred(Qs,N,PK)).
	
renameBody([],[],[]).
renameBody([B|Bs],[state([A|As],P/N)|Args],[B1|Bs1]) :-
	atom(A),
	!,
	getStateName([A|As],N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).
renameBody([B|Bs],[state([A],P/N)|Args],[B1|Bs1]) :- % singleton set
	!,
	getStateName(A,N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).
renameBody([B|Bs],[state(Qs,P/N)|Args],[B1|Bs1]) :-
	!,
	getStateName(Qs,N,P,Q),
	B =.. [_|Xs],
	B1=.. [Q|Xs],
	renameBody(Bs,Args,Bs1).

	
load_fta(File,Trs) :-
	open(File,read,S),
	read(S,Term),
	readTransitions(Term,S,Trs),
	close(S).
	
readTransitions(end_of_file,_,[]).
readTransitions((L -> R), S,[transition(Ys,state(R,P/N),C)|Trs]) :-
	L =.. [C|Xs],
	my_clause(H,B,C),
	separate_constraints(B,_,Bs),
	functor(H,P,N),
	addBodyArities(Bs,Xs,Ys),
	(member(errortrace,R) -> 
		true; 
		assertz(transition(Ys,state(R,P/N),C))),
	read(S,T),
	readTransitions(T,S,Trs).

addBodyArities([],[],[]).
addBodyArities([B|Bs],[Q|Qs],[state(Q,P/N)|Qs1]) :-
	functor(B,P,N),
	addBodyArities(Bs,Qs,Qs1).
	
newPredName(P,K,PK) :-
	name(P,QQ),
	name(K,KN),
	append(QQ,[95|KN],QKN),
	name(PK,QKN).
	
getCounter(K) :-
	retract(nameCounter(K)),
	K1 is K+1,
	assertz(nameCounter(K1)).
	
writeClauses(S) :-
	new_clause(A,Body),
	numbervars((A,Body),0,_),
	writeq(S,A),
	write(S,' :- '),
	nl(S),
	writeBodyClauses(Body,S),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_).
	
writeBodyClauses([],S) :-
	write(S,'      '),
	write(S,true).
writeBodyClauses([A],S) :-
	!,
	write(S,'      '),
	writeq(S,A).
writeBodyClauses([A|As],S) :-
	write(S,'      '),
	writeq(S,A),
	write(S,','),
	nl(S),
	writeBodyClauses(As,S).
	

readSplits(S,Spls) :-
	read(S,Term),
	readProcSplits(Term,S,Spls),
	close(S).
	
readProcSplits(end_of_file,_,[]).
readProcSplits(proc(A,B), S,[proc(A,B)|Spls]) :-
	read(S,T),
	readProcSplits(T,S,Spls).
	
errorSharePreds([proc(P,Ids)|Ps],[proc(P,Ids)|Es]) :-
	errorShare(P),
	!,
	errorSharePreds(Ps,Es).
errorSharePreds([_|Ps],Es) :-
	errorSharePreds(Ps,Es).
errorSharePreds([],[]).

errorShare(P/N) :-
	transition(_,state([_,_|_],P/N),_). % exists state containing P/N and some other pred.
	
	