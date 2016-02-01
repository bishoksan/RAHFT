:- module(canonical,
	[canonical/1, 
	 canonical_each/1,
	 melt/2, 
	 melteach/2,
	 variable/1], [assertions, isomodes, doccomments]).

%! @title Ground representation of terms

:- use_module(library(write), [numbervars/3]).

%! canonical(?T): instantiate `T` variables to obtain its ground
%    representation (destructively).
canonical(T) :-
	numbervars(T,0,_).

%! canonical_each(?Xs): @pred{canonical/1} on each element of `Xs`
canonical_each([]).
canonical_each([X|Xs]) :-
	canonical(X),
	canonical_each(Xs).

%! melt(+X,-Y): Obtain the free variable term representation `Y` from
%    ground representation `X`. E.g.,
%
%        ?- melt(a('$VAR'(1),'$VAR'(2),'$VAR'(1)),X).
%       
%        X = a(_A,_,_A) ? 
melt(X,Y) :-
	melt1(X,Y,_Assoclist),
	!.

melt1(X,Y,S) :-
	variable(X),
	!,
	assoc(X,Y,S).
melt1(X,X,_) :-
	atomic(X),
	!.
melt1(X,Y,S) :-
	functor(X,F,N),
	functor(Y,F,N),
	meltargs(1,N,X,Y,S).

meltargs(I,N,_,_,_) :-
	I > N,
	!.
meltargs(I,N,X,Y,S) :-
	arg(I,X,Xi),
	melt1(Xi,Yi,S),
	arg(I,Y,Yi),
	I1 is I+1,
	meltargs(I1,N,X,Y,S).


assoc(X,Y,[assoc(X,Y)|_]) :-
	!.
assoc(X,Y,[_|S]) :-
	assoc(X,Y,S).

%! variable(+X): `X` is a variable in ground representation
variable('$VAR'(_)).	

%! melteach(+Xs,-Ys): apply `melt/2` for each element
melteach([],[]).
melteach([X|Xs],[Y|Ys]) :-
	melt(X,Y),
	melteach(Xs,Ys).