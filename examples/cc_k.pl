% base case: that is a hit
cc(0, Y, 1,0) :- Y>0.

% base case: that is a miss
cc(X, _, 0,0) :- X<0.
cc(_, Y, 0,0) :- Y=<0.

%inductive case
cc(X, Y, Z,K) :- 
	X>0, kinds_of_coins(Y,A), X1 = X-A, cc(X1, Y, Z1,K1), Y1 = Y-1, cc(X, Y1, Z2,K2), Z = Z1 + Z2, 
	k4(K1,K2,K).

/*
kinds_of_coins(1,1).
kinds_of_coins(2,5).
kinds_of_coins(3,10).
kinds_of_coins(4,25).
kinds_of_coins(5,50).
*/

kinds_of_coins(A,B) :-
	A >= 1,
	B >= 1.
	
k4(K1,K2,K) :-
	K1=K2,
	K=K1+1.
k4(K1,K2,K1) :-
	K1>K2.
k4(K1,K2,K2) :-
	K1<K2.

% the book says that for amount 100 the number is 292
%false :- cc(100, 5, N), N > 292.
%
%false :- cc(A, 5, C), C>A, A>0.
%
%false :- cc(N, 5, N), N>1.

false :- B>=1, K > B, cc(A, B, C,K).