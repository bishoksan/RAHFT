:- module(duplVar, [writeAtomEq/4], []).

%?- writeAtomEq(p(U,U,V,U,V,W),A,Es).

%A = p(U,_A,V,_B,_C,W),
%Es = [U=_A,U=_B,V=_C] 

writeAtomEq(A,A1,Eqs0,Eqs1) :-
	A =.. [P|Xs],
	removeDupls(Xs,Xs1,Eqs0,Eqs1),
	A1 =.. [P|Xs1].
	
removeDupls([],[],Es,Es).
removeDupls([X|Xs],Xs2,[X=Y|Eqs0],Eqs1) :-
	replaceDupl(X,Xs,Y,Xs1),
	!,
	removeDupls([X|Xs1],Xs2,Eqs0,Eqs1).
removeDupls([X|Xs],[X|Xs1],Eqs0,Eqs1) :-
	removeDupls(Xs,Xs1,Eqs0,Eqs1).

replaceDupl(X1,[X2|Xs],XK,[XK|Xs]) :-
	X1 == X2,
	!.
replaceDupl(X,[X1|Xs],Y,[X1|Xs1]) :-
	replaceDupl(X,Xs,Y,Xs1).