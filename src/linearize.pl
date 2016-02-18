:- module(linearize,[linearize/2,  linear_constraint/1], []).

:- use_module(library(lists)).


:- include(common).

recognised_option(_,_,_). % this is needed due to common.pl include
	
%%%%%%%%%%%%
%
% Linearise
%
%%%%%%%%%%%%

linearize([],[]).
linearize([C|Cs],[C|Cs1]) :-
	linear_constraint(C),
	!,
	linearize(Cs,Cs1).

%Version with linear approx. goes here

linearize([C|Cs],[C1|Cs1]) :-
	linear_approx(C,C1),
	!,
	linearize(Cs,Cs1).

linearize([_C|Cs],Cs1) :-
		linearize(Cs,Cs1).
		
linear_constraint(X = Y) :-
	linear_term(X),
	linear_term(Y).
	
linear_constraint(X =:= Y) :-
	linear_term(X),
	linear_term(Y).

linear_constraint(X is Y) :-
	linear_term(X),
	linear_term(Y).

linear_constraint(X>Y) :-
	linear_term(X),
	linear_term(Y).

linear_constraint(X<Y) :-
	linear_term(X),
	linear_term(Y).

linear_constraint(X>=Y) :-
	linear_term(X),
	linear_term(Y).

linear_constraint(X=<Y) :-
	linear_term(X),
	linear_term(Y).

		
linear_term(X) :- 
	const(X),!.

linear_term(X) :- 
	var(X),!.

linear_term(-(X)) :- 
	linear_term(X).
	
linear_term(+(X)) :- 
	linear_term(X).
	
linear_term(X+Y) :- 
	linear_term(X),
	linear_term(Y).

linear_term(X-Y) :- 
	linear_term(X),
	linear_term(Y).

linear_term(X*Y) :- 
	const(X),
	linear_term(Y);
	const(Y),
	linear_term(X).
	
linear_term(X/Y) :- 
	const(Y),
	linear_term(X).

const(X) :-
	number(X),!.

const(X) :-
	var(X),!,fail.

const(X+Y) :-
	const(X),
	const(Y).			

const(X-Y) :-
	const(X),
	const(Y).			

const(X*Y) :-
	const(X),
	const(Y).			

const(X/Y) :-
	const(X),
	const(Y).					

%Discard lists
%linear_approx(X = [H|T],0=0) :- !, write('Discarding list'),nl.

linear_approx(X = Y \/ Z,X = Const) :-
	const(Y),const(Z),Const is Y \/ Z,!.

linear_approx(X = Y \/ Z, X =< Y + Z) :-
       const(X),!.


%Should be =< and not X < Y + Z
linear_approx(X = Y \/ Z, X =< Y + Z) :- !.

%
%AND
%

linear_approx(X = Y /\ Z,X = Const) :-
	const(Y),const(Z),Const is Y /\ Z,!.
linear_approx(X = Y /\ Z,X =< Y + Z) :-
	const(X),!.

linear_approx(X = Y /\ Z, X =< Y + Z) :- !.

%
% XOR
%

linear_approx(X = Y # Z,X = Const) :-
        const(Y),const(Z),!, Const is Y # Z.

%Do not remove variables! ..
linear_approx(X = Y # Z, X < 256 + Y-Y) :- const(Z),!.
linear_approx(X = Y # Z, X < 256 + Y+Z) :- !.

%Shift right

linear_approx(X = Y>>Z,X = W) :-
 	const(Y),const(Z),
 	W is Y>>Z,!.

%linear_approx(X = Y>>Z,X=Y/2^Z) :-
%	const(Z).

linear_approx(X = Y>>_,X=0) :- 
	const(Y),Y is 0, !.

linear_approx(X = _>>Z,X=0) :-
	const(Z), Z >= 8, !.

linear_approx(X = Y>>Z,Y<256) :- 
	const(X), X is 0, const(Z), Z >= 8,!.

linear_approx(X = Y>>Z,X>=Y) :-
	const(Z),!.

%Unprecise but simple
%linear_approx(X = Y << Z, X >= Y).

%Introducing an extra variable (T1) is a problem...
linear_approx(X = Y << Z,X=Y*T1) :-
	const(Z), !,
	T1 is truncate(2 ** Z).

linear_approx(X = Y ** Z,X=T1) :-
        const(Y),
	const(Z), !,
	T1 is truncate(Y ** Z).


%At the moment, it is not needed to separate const. Y from var. Y
linear_approx(X = \Y ,X=255-Y) :-
	var(Y), !.

linear_approx(X = \Y, X = 255 - Y) :-
	const(Y), !.
	


