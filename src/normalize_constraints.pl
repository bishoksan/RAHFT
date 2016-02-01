% converts const into AX<=b
:- module(normalize_constraints, _, []).

:- use_module(library(write)).
:- use_module(library(lists)).

go:-
    numbervars(A,0,_),
    normalizeConstraints([1*A < 90], [A], C),
    write(C).

normalizeConstraints([], _,[]).
normalizeConstraints([C|Cs], VarList, Cs2):-
	normalizeConstraint(C, VarList,C1),
	!,
	normalizeConstraints(Cs, VarList,Cs1),
	append(C1,Cs1,Cs2).	
			
normalizeConstraint(T1=<T2, VarList, C):-
	normalizeLC(T1, VarList, L1, R1),
	normalizeRC(T2, VarList, L2, R2),
	append(L1,L2, Left),
	append(R1,R2, Right),
	list2NonList(Left,LeftNL),
	list2NonList(Right,RightNL),
	C= [LeftNL =< RightNL].
normalizeConstraint(T1<T2, VarList, C):-
	normalizeLC(T1, VarList, L1, R1),
	normalizeRC(T2, VarList, L2, R2),
	append(L1,L2, Left),
	append(R1,R2, Right),
	list2NonList(Left,LeftNL),
	list2NonList(Right,RightNL),
    C= [LeftNL  <  RightNL].
%this is valid over integer
    %RightNL1 is RightNL - 1,
	%C= [LeftNL  =<  RightNL1].
normalizeConstraint(T1>T2, VarList, C):-
	normalizeConstraint(T2<T1, VarList, C).
normalizeConstraint(T1>=T2, VarList, C):-
	normalizeConstraint(T2=<T1, VarList, C).
normalizeConstraint(T1=T2, VarList, C):-
	normalizeConstraints([T2=<T1,T1=<T2 ], VarList, C).	
			

normalizeConstraint1(-(0), _, 0):-
	!.	
normalizeConstraint1(T1, _, N1):-
	formatNumber(T1,N1),
	number(N1),
	!.
normalizeConstraint1(-(T1), VarList, -1* C1):-
	normalizeConstraint1(T1, VarList, C1),
	!.	
normalizeConstraint1(- (-A * T1), VarList, A* C1):-
	normalizeConstraint1(T1, VarList, C1),
	!.	
normalizeConstraint1(- (A * T1), VarList, -A* C1):-
	normalizeConstraint1(T1, VarList, C1),
	!.	
normalizeConstraint1( -(T1 + T2) , VarList,  C1 + C2):-
	normalizeConstraint1(- T1, VarList, C1),
	normalizeConstraint1(- T2, VarList, C2).
normalizeConstraint1( -(T1 - T2) , VarList,  C1 + C2):-
	normalizeConstraint1(- T1, VarList, C1),
	normalizeConstraint1(T2, VarList, C2).

normalizeConstraint1(T1, _, T1).

normalizeLC(T1 + T2, VarList, Left, Right):-
	!,
	normalizeLC(T1, VarList, L1, R1),
	normalizeLC(T2, VarList, L2, R2),
	addLeft(L1,L2, Left),
	addRight(R1,R2,Right).	
normalizeLC(T1 - T2, VarList,Left, Right):-
	!,
	normalizeConstraint1(-T2, VarList,T2N),
	normalizeLC(T1 + T2N, VarList, Left, Right).
	%normalizeLC(T1, VarList, L1, R1),
	%normalizeLC(T2, VarList, L2, R2),
	%subLeft(L1,L2, Left),
	%subRight(R1,R2,Right).
normalizeLC(- T1, _, [], [N]):-
	formatNumber(T1, N),
	number(N),
	!.
normalizeLC(T1, _, [], [N]):-
	number(T1),
	formatNumber(-T1, N), 
	!.
normalizeLC(T1, VarList, [T1], []):-
	member2(T1, VarList),
	!.	
normalizeLC(A*T1, VarList, [N*T1], []):-
	member2(T1, VarList),
	formatNumber(A, N), 
	!.		
	

normalizeRC(T1 + T2, VarList, Left, Right):-
	normalizeRC(T1, VarList, L1, R1),
	normalizeRC(T2, VarList, L2, R2),
	addLeft(L1,L2, Left),
	addRight(R1,R2,Right).
normalizeRC(T1 - T2, VarList, Left, Right):-
	normalizeConstraint1(-T2, VarList,T2N),
	normalizeRC(T1 + T2N, VarList, Left, Right).
	%normalizeRC(T1, VarList, L1, R1),
	%normalizeRC(T2, VarList, L2, R2),
	%subLeft(L1,L2, Left),
	%subRight(R1,R2,Right).	
normalizeRC(-T2, VarList, [T2], []):-
	member2(T2, VarList),
	!.
normalizeRC(T2, _, [], [N]):-
	formatNumber(T2, N),
	number(N), 
	!.
normalizeRC(T2, VarList, [-1*T2], []):-
	member2(T2, VarList),
	!.

normalizeRC(-A*T2, VarList, [N*T2], []):-
	member2(T2, VarList),
	formatNumber(A, N), 
	!.		
normalizeRC(A*T2, VarList, [N*T2], []):-
	member2(T2, VarList),
	formatNumber(-A, N), 
	!.	

	
addLeft(L1,[], L1):-!.
addLeft([],L2, L2):-!.
addLeft(L1,L2, [L]):-
	list2NonList(L1, L1N),
	list2NonList(L2, L2N),
	L= L1N + L2N.
	
addRight(L1,[], L1):-!.
addRight([],L2, L2):-!.
addRight(L1,L2, [L]):-
	list2NonList(L1, L1N),
	list2NonList(L2, L2N),
	L is L1N + L2N.	

subLeft(L1,[], L1):-
!.
subLeft([],L2, [L3]):- 
	list2NonList(L2, L2NL),
	L3 is 0 - L2NL,
	!.
subLeft(L1,L2, [L]):-
	list2NonList(L1, L1N),
	list2NonList(L2, L2N),
	L= L1N - L2N.
	
subRight(L1,[], L1):-!.
subRight([],L2, [L3]):- 
	list2NonList(L2, L2NL),
	L3 is 0 - L2NL,
	!.
subRight(L1,L2, [L]):-
	list2NonList(L1, L1N),
	list2NonList(L2, L2N),
	L is L1N - L2N.	
	
	
list2NonList([],0) :-
	!.
list2NonList([A],A) :-
	!.
list2NonList([A|As],L) :-
	list2NonList(As,As1),
	 L= A + As1.

list2conj([],0) :-
	!.
list2conj([A],A) :-
	!.	
list2conj([A|As],L) :-
	list2conj(As,As1),
	L= (A,As1).		

formatNumber(+(N), N):-number(N),!.
formatNumber(-(N), K):-number(N), K is 0-N,!.
formatNumber(N,N):-number(N),!.

member2(X, [X|_]):-!.        
                        
member2(X, [_|Tail]) :-   
  member2(X, Tail).
  
  

/*  
remove_paren(A+(B), A+B).
remove_paren((A)+B, A+B).
remove_paren((A)+(B), A+B).
remove_paren(A,A).
*/
 
	      			
			
