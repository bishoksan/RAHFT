:- module(reljoin,[relJoin/4,test1/1,test2/1,test3/1], []).

% Finds the solution of a conjunction, eliminating local variables
% Sorts each relation first by its non-locals.

:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(sort), [keysort/2]).

:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(setops)).

relJoin(Bs,H,Ys,Sols) :-
	sortEachRel(Bs,Ys,Rs1),
	productRel(Rs1,H,Sols).
	
sortEachRel((member(B,BSols),Bs),Ys,[rel(Ws,Zs,BS)|BSSs]) :-
	!,
	sortRel(B,BSols,Ys,Ws,Zs,BS),
	sortEachRel(Bs,Ys,BSSs).
sortEachRel(true,_,[]) :-
	!.
sortEachRel(member(B,BSols),Ys,[rel(Ws,Zs,BS)]) :-
	sortRel(B,BSols,Ys,Ws,Zs,BS).

sortRel(B,BSols,Ys,Ws,Zs,BS) :-
	vars(B,Xs),
	setdiff(Xs,Ys,Ws),		% head vars in B
	setintersect(Xs,Ys,Zs),		% local vars in B
	findall(Ws-locals(Zs),member(B,BSols),BSols1),
	keysort(BSols1,BSols2),
	consolidate(BSols2,BS).
	
consolidate([X-locals(Ys)|Zs],[item(X,[Ys|Qs])|Ws]) :-
	collectset(Zs,X,Qs,Zs1),
	consolidate(Zs1,Ws).
consolidate([],[]).

collectset([],_,[],[]).
collectset([X-locals(Ys)|Zs],X1,[Ys|Qs],Zs1) :-
	X==X1,
	!,
	collectset(Zs,X,Qs,Zs1).
collectset([X1-Q|Zs],_,[],[X1-Q|Zs]).
	
productRel(Rs1,H,Sols) :-
	%productcount(Rs1,N),
	%write('potential product size: '), write(N),nl,
	findall(H,(
			tuple(Rs1,Locals),
			oneSolution(Locals)),
		Sols).
		
productcount([],1).
productcount([rel(_,_,B)|Rs],N) :-
	length(B,K),
	productcount(Rs,M),
	N is M*K.
	
tuple([],[]).
tuple([rel(Ws,Xs,B)|Bs],[locals(Xs,L)|Ls]) :-
	member(item(Ws,L),B),
	forwardCheckTuples(Bs,Bs1),
	tuple(Bs1,Ls).
	
oneSolution(Ls) :-
	forwardCheckLocals(Ls,Ls1),
	localSolution(Ls1),
	!.		% one soln is enough
	
localSolution([locals(Xs,L)|Ls]) :-
	member(Xs,L),
	forwardCheckLocals(Ls,Ls1),
	localSolution(Ls1).
localSolution([]).

forwardCheckTuples([rel(Ws,Xs,B)|Ts], [rel(Ws,Xs,B1)|Ts1]) :-
	pruneItems(B,Ws,B1),
	checkDetItem(B1,Ws),
	forwardCheckTuples(Ts,Ts1).
forwardCheckTuples([],[]).

forwardCheckLocals([locals(Xs,L)|Ls], Ls2) :-
	prune(L,Xs,L1),
	checkDet(L1,Xs,Ls1,Ls2),
	forwardCheckLocals(Ls,Ls1).
forwardCheckLocals([],[]).

prune([Y|Ys],X,Ys1) :-
	X \= Y , 	% prune elements that don't unify with X
	!,
	prune(Ys,X,Ys1).
prune([Y|Ys],X,[Y|Ys1]) :-
	prune(Ys,X,Ys1).
prune([],_,[]).

pruneItems([Y|Ys],X,Ys1) :-
	item(X,_) \= Y , 	% prune elements that don't unify with X
	!,
	pruneItems(Ys,X,Ys1).
pruneItems([Y|Ys],X,[Y|Ys1]) :-
	pruneItems(Ys,X,Ys1).
pruneItems([],_,[]).

checkDet([Xs1],Xs,Ls1,Ls1) :-
	!,	% if singleton select immediately
	Xs1=Xs.
checkDet([Y|Ys],Xs,Ls1,[locals(Xs,[Y|Ys])|Ls1]).

checkDetItem([Xs1],Xs) :-
	!,	% if singleton select immediately
	Xs1=item(Xs,_).
checkDetItem([_|_],_).

	
test1(Sols) :-
	relJoin(
		(member(p(A,B,Y1),
			[p(a,b,c),p(a,c,c),p(b,d,e)]),
     		member(q(A,Y1,Y2),
     			[q(a,c,f)]),
     		member(r(Y2),
     			[r(c),r(g),r(h),r(f)])),
     		h(A),[B,Y1,Y2],Sols).
	
test2(Sols) :-
	relJoin(
		(member(p(A,B,Y1),
			[p(_,b,c),p(_,c,c),p(b,d,e)]),
     		member(q(A,Y1,Y2),
     			[q(_,c,f),q(a,d,f)]),
     		member(r(Y2,B),
     			[r(c,d),r(g,c),r(h,h),r(f,c)])),
     		h(A,B),[Y1,Y2],Sols).
     		
test3(Sols) :-
	relJoin(
		(member(p(A,B),
			[p(X,X)]),
     		member(q(B,C),
     			[q(b,c),q(Y,Y)])),
     		h(A,C),[B],Sols).

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
