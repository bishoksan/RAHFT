:- module(raf, [main/1, raf/3], []).

% JPG - RAF algorithm.  Leuschel & Jørgensen 1996
%
% Redundant argument filtering

:- use_module(chclibs(builtins)).
:- use_module(chclibs(scc)).
:- use_module(chclibs(balanced_tree)).
:- use_module(chclibs(readprog)).

:- use_module(chclibs(timer_ciao)).

:- use_module(chclibs(canonical)).
:- use_module(chclibs(myterms)).
:- use_module(chclibs(setops)).

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(write)).

:- use_module(reljoin).
:- use_module(cartprod).

:- dynamic(topGoal/1).

%topGoal(fact__0/2).
%topGoal(member/2).
%topGoal(false/0).

main([File,Entry,Outfile]) :-
	raf(File,Entry,Outfile).

raf(F,Entry,OutF) :-
	retractall(topGoal(_)),
	convertQueryString(Entry,Entry1),
	functor(Entry1,P,N),
	assert(topGoal(P/N)),
	readprog(F,[predicates(Ps)|Cls]),
	initFilters(Ps,R0),			% start with all args that might be erasable. 
	insertFilters(R0,root,F0),
	raf_fix(changed,Cls,F0,Filters),
	filterProg(Cls,Filters,Cls1),
	open(OutF,write,S),
	writeClauses(Cls1,S),
	close(S).

raf_fix(changed,Cls,F0,F2) :-
	%displayState(F0),
	raf_iteration(Cls,F0,Es,[]),
	addErasures(Es,F0,F1,nochange,Ch),
	raf_fix(Ch,Cls,F1,F2).
raf_fix(nochange,_,F0,F0).
	
raf_iteration([clause((H :- B),_)|Cls],F0,Es0,Es2) :-
	varseq(B,Xs),
	filterAtom(H,F0,H1),
	varseq(H1,Vs),
	multipleOccurringVars(Xs,Us),
	erasures(B,Us,Vs,Es0,Es1),
	raf_iteration(Cls,F0,Es1,Es2).
raf_iteration([],_,Es,Es).

erasures((B,Bs),Us,Vs,Es0,Es2) :-
	!,
	atomErasures(B,Us,Vs,Es0,Es1),
	erasures(Bs,Us,Vs,Es1,Es2).
erasures(B,Us,Vs,Es0,Es2) :-
	!,
	atomErasures(B,Us,Vs,Es0,Es2).
	
atomErasures(B,_,_,Es0,Es0) :-
	sp_builtin(B),
	!.
atomErasures(B,Us,Vs,Es0,Es1) :-
	functor(B,P,N),
	checkErase(N,B,P/N,Us,Vs,Es0,Es1).
	
checkErase(J,_,_,_,_,Es0,Es0) :-
	J =< 0.
checkErase(J,B,P/N,Us,Vs,Es0,Es2) :-
	J > 0,
	arg(J,B,X),
	eraseCondition(X,Us,Vs,P/N,J,Es0,Es1),
	J1 is J-1,
	checkErase(J1,B,P/N,Us,Vs,Es1,Es2).
	
eraseCondition(X,_,_,P/N,J,[(P/N,J)|Es0],Es0) :-
	notVar(X),
	!.
eraseCondition(X,Us,_,P/N,J,[(P/N,J)|Es0],Es0) :-
	member(X,Us),
	!.
eraseCondition(X,_,Vs,P/N,J,[(P/N,J)|Es0],Es0) :-
	member(X,Vs),
	!.
eraseCondition(_,_,_,_,_,Es0,Es0).

notVar(X) :-
	\+ variable(X).
	
addErasures([],F0,F0,Ch,Ch).
addErasures([(P,J)|R1],F0,F2,Ch0,Ch2) :-
	search_replace_tree(F0,P,(P :- S1),F1,(P :- S2)),
	!,
	removeErasedArg(S1,J,S2,Ch0,Ch1),
	addErasures(R1,F1,F2,Ch1,Ch2).
	
insertFilters([],F0,F0).
insertFilters([(P:-S)|R1],F0,F2) :-
	!,
	insert_tree(F0,P,(P:-S),F1),
	insertFilters(R1,F1,F2).

removeErasedArg([],_,[],Ch,Ch) :-
	!.
removeErasedArg([J|Js],J,Js,_,changed) :-
	!.
removeErasedArg([J1|Js],J,[J1|Js1],Ch0,Ch1) :-
	removeErasedArg(Js,J,Js1,Ch0,Ch1).
	
initFilters([P/N|Ps],[(P/N :- [])|R]) :-
	functor(B,P,N),
	sp_builtin(B),
	!,
	initFilters(Ps,R).
initFilters([P/N|Ps],[(P/N :- [])|R]) :-
	topGoal(P/N),
	!,
	initFilters(Ps,R).
initFilters([P/N|Ps],[(P/N :- Js)|R]) :-
	argNums(N,Js),
	initFilters(Ps,R).
initFilters([],[]).

argNums(0,[]).
argNums(J,[J|Js]) :-
	J > 0,
	J1 is J-1,
	argNums(J1,Js).


applyfilters((B,Bs),R,[B|BVs]) :-
	sp_builtin(B),
	!,
	applyfilters(Bs,R,BVs).
applyfilters((B,Bs),R,[Vs|BVs]) :-
	!,
	functor(B,P,N),
	search_tree(R,P/N,(P/N :- Zs)),
	selectArgs(B,N,Zs,Vs),
	applyfilters(Bs,R,BVs).
applyfilters(B,_,[B]) :-
	sp_builtin(B),
	!.
applyfilters(B,R,[Vs]) :-
	functor(B,P,N),
	search_tree(R,P/N,(P/N :- Zs)),
	selectArgs(B,N,Zs,Vs).
	
selectArgs(_,J,_,[]) :-
	J < 1.
selectArgs(B,J,Zs,Vs) :-
	J >= 1,
	member(J,Zs), 	% non-filtered arg
	!,
	J1 is J-1,
	selectArgs(B,J1,Zs,Vs).
selectArgs(B,J,Zs,[T|Vs]) :-
	J1 is J-1,
	arg(J,B,T),
	selectArgs(B,J1,Zs,Vs).
	
collectFilteredArgs(_,J,_,_,[]) :-
	J < 1,
	!.
collectFilteredArgs(H,J,Us,Vs,Ys) :-
	%J >= 1,
	J1 is J-1,
	arg(J,H,A),
	filteredArg(A,Us,Vs,J,Ys,Ys1),
	collectFilteredArgs(H,J1,Us,Vs,Ys1).
	

filteredArg(A,_,_,J,[J|Ys1],Ys1) :-		% rule 1
	nonvar(A),
	!.
filteredArg(A,Us,_,J,[J|Ys1],Ys1) :-	% rule 2
	var(A),
	memb3(A,Us),
	!.
filteredArg(A,_,Vs,J,[J|Ys1],Ys1) :-	% rule 3
	var(A),
	memb3(A,Vs),
	!.
filteredArg(_,_,_,_,Ys1,Ys1).


memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).

memb3(X,[X1|_]) :-
	X == X1,
	!.
memb3(X,[_|Xs]) :-
	memb3(X,Xs).

vars(T,Vs) :-
        vars3(T,[],Vs).

vars3(X,Vs,Vs1) :-
        var(X),
        !,
        insertvar(X,Vs,Vs1).
vars3(X,Vs,Vs) :-
	atomic(X),
	!.
vars3(X,Vs,Vs1) :-
        nonvar(X),
        X =.. [_|Args],
        argvars(Args,Vs,Vs1).
 
argvars([],Q,Q).
argvars([X|Xs],Vs,Vs2) :-
        vars3(X,Vs,Vs1),
        argvars(Xs,Vs1,Vs2).
 
insertvar(X,[],[X]).
insertvar(X,[Y|Vs],[Y|Vs]) :-
        X == Y,
        !.
insertvar(X,[Y|Vs],[Y|Vs1]) :-
        insertvar(X,Vs,Vs1).


writeList([],_).
writeList([X|Xs],S) :-
	write(S,X),
	writeList(Xs,S).
	
varseq(X,S) :-
	varseq3(X,S,[]).
	
varseq3(X,[X|S],S) :-
	variable(X),
	!.
varseq3(X,S0,S2) :-
	X =.. [_|Xs],
	varseqList(Xs,S0,S2).
	
varseqList([],S0,S0).
varseqList([X|Xs],S0,S2) :-
	varseq3(X,S0,S1),
	varseqList(Xs,S1,S2).
	
multipleOccurringVars([],[]).
multipleOccurringVars([_],[]).
multipleOccurringVars([X1,X2|Xs],[X1|Ys]) :-
	memb3(X1,[X2|Xs]),
	!,
	multipleOccurringVars([X2|Xs],Ys).
multipleOccurringVars([_,X2|Xs],Ys) :-
	multipleOccurringVars([X2|Xs],Ys).
	
filterProg([predicates(Ps)|Cls],Filters,[predicates(Ps)|Cls1]) :-
	filterProg(Cls,Filters,Cls1).
filterProg([clause((H :- B),Ws)|Cls],Filters,[clause((H1 :- B1),Ws)|Cls1]) :-
	filterAtom(H,Filters,H1),
	filterBody(B,Filters,B1),
	filterProg(Cls,Filters,Cls1).
filterProg([],_,[]).

filterAtom(H,Filters,H1) :-
	functor(H,P,N),
	search_tree(Filters,P/N,(P/N :- F)),
	H =.. [P|Xs],
	filterArgs(Xs,1,N,F,Ys),
	H1 =.. [P|Ys].
	
filterBody((B,Bs),Filters,(B,Bs1)) :-
	sp_builtin(B),
	!,
	filterBody(Bs,Filters,Bs1).
filterBody((B,Bs),Filters,(B1,Bs1)) :-
	!,
	filterAtom(B,Filters,B1),
	filterBody(Bs,Filters,Bs1).
filterBody(B,_,B) :-
	sp_builtin(B).
filterBody(B,Filters,B1) :-
	filterAtom(B,Filters,B1).
	
filterArgs([],_,_,_,[]).
filterArgs([_|Xs],J,N,F,Ys) :-
	member(J,F),
	!,
	J1 is J+1,
	filterArgs(Xs,J1,N,F,Ys).
filterArgs([X|Xs],J,N,F,[X|Ys]) :-
	J1 is J+1,
	filterArgs(Xs,J1,N,F,Ys).
	
orderedMember(X,[X|_]).
orderedMember(X,[Y|Ys]) :-
	X < Y,
	orderedMember(X,Ys).
	
displayState(F0) :-
	traverse_tree(F0,Fs),
	writeRecs(Fs).
	
writeRecs([rec(_,V)|Rs]) :-
	write(V),
	nl,
	writeRecs(Rs).
writeRecs([]).

% TODO: read_from_atom/2 is buggy!
:- use_module(library(read_from_string), [read_from_atom/2]).

convertQueryString(Q,Q1) :-
	read_from_atom(Q,Q1).
