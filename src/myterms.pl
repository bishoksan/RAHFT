:- module(myterms,[instanceOf/2,
		variantOf/2,
		meltedInstance/2,
		copyterm/2,
		instanceIndex/2,
		instanceIndex1/2,
		removeDupls/2,
		pruneAtoms/2,
		pruneSolns/2], []).

%%%CIAO/*
:- use_module(library(terms_check)).
:- use_module(library(terms_vars)).
%%%CIAO*/
/*%%%SICS
:- use_module(library(terms)).
*/%%%SICS
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(canonical).

copyterm(X,Y) :-
	findall(X,X=_,[Y]).

% remove all subsumed atoms from a list
% assume there are no duplicates
% canonical form in, melted form out
% Arguments of atoms are either ground or var.

%pruneAtoms(As,Bs) :-	
%	melteach(As,As1),
%	separateLevels(As1,[],Ls),
%	pruneLevels(Ls,Bs).
	
% More general definition of pruneAtoms, as above
% but without the ground-var argument restriction. 

pruneAtoms(As,Bs) :-
	melteach(As,As1),
	separateLevels1(As1,[],Ls),
	pruneLevels(Ls,Bs).
	
	
separateLevels([],Ls,Ls).
separateLevels([A|As],Ls0,Ls2) :-
	instanceIndex1(A,N),
	insertLevel(Ls0,N,A,Ls1),
	separateLevels(As,Ls1,Ls2).
	
separateLevels1([],Ls,Ls).
separateLevels1([A|As],Ls0,Ls2) :-
	instanceIndex2(A,N),
	insertLevel(Ls0,N,A,Ls1),
	separateLevels1(As,Ls1,Ls2).
	
insertLevel([],N,A,[N-[A]]).
insertLevel([N-Xs|Ls],N,A,[N-[A|Xs]|Ls]).
insertLevel([N1-Xs|Ls],N,A,[N-[A],N1-Xs|Ls]) :-
	N < N1.
insertLevel([N1-Xs|Ls],N,A,[N1-Xs|Ls1]) :-
	N > N1,
	insertLevel(Ls,N,A,Ls1).
	
pruneLevels([],[]).
pruneLevels([_-Xs|Ls],Bs) :-
	pruneLevelN(Ls,Xs,Bs).

pruneLevelN([],Bs,Bs).
pruneLevelN([_-Xs|Ls],Ws,Bs) :-
	elimSubsumed(Xs,Ws,Ws1,Ws),
	%append(Xs1,Ws,Ws1),
	pruneLevelN(Ls,Ws1,Bs).


elimSubsumed([],_,Ws,Ws).
elimSubsumed([X|Xs],Ws,Ws0,Ws1) :-
	member(W,Ws),
	meltedInstance(X,W),
	!,
	elimSubsumed(Xs,Ws,Ws0,Ws1).
elimSubsumed([X|Xs],Ws,[X|Ws0],Ws1) :-
	elimSubsumed(Xs,Ws,Ws0,Ws1).

/*
elimSubsumed([],_,[]).
elimSubsumed([X|Xs],Ws,Ws0) :-
	member(W,Ws),
	meltedInstance(X,W),
	!,
	elimSubsumed(Xs,Ws,Ws0).
elimSubsumed([X|Xs],Ws,[X|Ws0]) :-
	elimSubsumed(Xs,Ws,Ws0).
	*/
	
%pruneAtoms(As,Bs) :-
%	nonGroundAtoms(As,NGs,Gs),
%	%length(NGs,Nn),length(Gs,Gn),write('NG='),write(Nn),write('G='),write(Gn),nl,
%	pruneNGatoms(NGs,NGs1),
%	pruneNGatoms(NGs1,NGs2),
%	pruneGatoms(Gs,Bs,NGs2).
        
pruneGatoms([X|Xs],Xs1,U) :-
	subsumed(X,U),
	!,
	pruneGatoms(Xs,Xs1,U).
pruneGatoms([X|Xs],[X|Xs1],U) :-
	pruneGatoms(Xs,Xs1,U).
pruneGatoms([],Tail,Tail).

pruneNGatoms([],[]).
pruneNGatoms([X|NGs],NGs1) :-
	subsumed(X,NGs),
	!,
	pruneNGatoms(NGs,NGs1).
pruneNGatoms([X|NGs],[X|NGs1]) :-
	pruneNGatoms(NGs,NGs1).
	
nonGroundAtoms([],[],[]).
nonGroundAtoms([A|As],NGs,[A|Gs]) :-
	myground(A),
	!,
	nonGroundAtoms(As,NGs,Gs).
nonGroundAtoms([A|As],[A|NGs],Gs) :-
	nonGroundAtoms(As,NGs,Gs).
	
myground(A) :-
	A=..[_|Xs],
	mygroundargs(Xs).

mygroundargs([]).
mygroundargs([X|Xs]) :-
	functor(X,F,_),
	F \== '$VAR',
	mygroundargs(Xs).
	
subsumed(X,[Y|_]) :-
	%X \== Y,
	instanceOf(X,Y),
	!.
subsumed(X,[_|L]) :-
	subsumed(X,L).

% Determine the level of a term in the term lattice
% Assumes that args are either variable or ground

instanceIndex1(A,K) :-
	A =.. [_|Xs],
	countBindings1(Xs,[],K).
	
countBindings1([],_,0).
countBindings1([X|Xs],Ws,K) :-
	nonvar(X),
	!,
	countBindings1(Xs,Ws,K1),
	K is K1+1.
countBindings1([X|Xs],Ws,K) :-
	var(X),
	vmemb(X,Ws),
	!,
	countBindings1(Xs,Ws,K1),
	K is K1+1.
countBindings1([X|Xs],Ws,K) :-
	countBindings1(Xs,[X|Ws],K).
	
vmemb(X,[X1|_]) :-
	X==X1,
	!.
vmemb(X,[_|Xs]) :-
	vmemb(X,Xs).

% Same as instanceIndex1 bu with ground representation
instanceIndex(A,K) :-
	A =.. [_|Xs],
	countBindings(Xs,[],K).
	
countBindings([],_,0).
countBindings([X|Xs],Ws,K) :-
	variable(X),
	member(X,Ws),
	!,
	countBindings(Xs,Ws,K1),
	K is K1+1.
countBindings([X|Xs],Ws,K) :-
	variable(X),
	!,
	countBindings(Xs,[X|Ws],K).
countBindings([_|Xs],Ws,K) :-
	countBindings(Xs,Ws,K1),
	K is K1+1.
	
instanceIndex2(A,N) :-
	termSize(A,K),
	termVars(A,Vs),
	length(Vs,J),
	N is K-J.
	
termSize(X,1) :-
	var(X),
	!.
termSize(X,N) :-
	X =.. [_|Xs],
	termSizeList(Xs,N1),
	N is N1+1.
	
termSizeList([],0).
termSizeList([X|Xs],N) :-
	termSize(X,M),
	termSizeList(Xs,N1),
	N is N1+M.

	
% operations on melted lists

pruneSolns(Xs,Ys) :-
	removeDupls(Xs,Ws),
	pruneAtoms1(Ws,Ys).
	
removeDupls(Sols1,Sols2) :-
	sort(Sols1,Sols3),
	elimDupls(Sols3,Sols2).
	
elimDupls([],[]).
elimDupls([A|As],[A|Bs]) :-
	elimDupls3(As,A,Bs).
	
elimDupls3([],_,[]).
elimDupls3([A1|As],A,Bs) :-
	A1==A,
	!,
	elimDupls3(As,A,Bs).
elimDupls3([A1|As],_,[A1|Bs]) :-
	elimDupls3(As,A1,Bs).
	
pruneAtoms1(As,Bs) :-
	nonGroundAtoms1(As,NGs,Gs),
	%length(NGs,Nn),length(Gs,Gn),write('NG='),write(Nn),write('G='),write(Gn),nl,
	pruneNGatoms1(NGs,NGs1),
	pruneNGatoms1(NGs1,NGs2),
	pruneGatoms1(Gs,Bs,NGs2).
        
pruneGatoms1([X|Xs],Xs1,U) :-
	subsumed1(X,U),
	!,
	pruneGatoms1(Xs,Xs1,U).
pruneGatoms1([X|Xs],[X|Xs1],U) :-
	pruneGatoms1(Xs,Xs1,U).
pruneGatoms1([],Tail,Tail).

pruneNGatoms1([],[]).
pruneNGatoms1([X|NGs],NGs1) :-
	subsumed1(X,NGs),
	!,
	pruneNGatoms1(NGs,NGs1).
pruneNGatoms1([X|NGs],[X|NGs1]) :-
	pruneNGatoms1(NGs,NGs1).

nonGroundAtoms1([],[],[]).
nonGroundAtoms1([A|As],NGs,[A|Gs]) :-
	ground(A),
	!,
	nonGroundAtoms1(As,NGs,Gs).
nonGroundAtoms1([A|As],[A|NGs],Gs) :-
	nonGroundAtoms1(As,NGs,Gs).
	
subsumed1(X,[Y|_]) :-
	meltedInstance(X,Y),
	!.
subsumed1(X,[_|L]) :-
	subsumed1(X,L).

%%%CIAO/*
instanceOf(X,Y) :-
	melt(X,X1),
	melt(Y,Y1),
	instance(X1,Y1).

meltedInstance(X,Y) :-
	instance(X,Y).
	
variantOf(X,Y) :-
	variant(X,Y).
	
termVars(T,Vs) :-
	varset(T,Vs).
%%%CIAO*/


/*%%%SICS
instanceOf(X,Y) :-
	melt(X,X1),
	melt(Y,Y1),
	subsumes_chk(Y1,X1).

meltedInstance(X,Y) :-
	subsumes_chk(Y,X).
	
variantOf(X,Y) :-
	variant(X,Y).
	
termVars(T,Vs) :-
	term_variables(T,Vs).
*/%%%SICS

