
cc(0, Y, 1) :- Y>0.

% base case: that is a miss
cc(X, _, 0) :- X<0.
cc(_, Y, 0) :- Y=<0.

%inductive case
cc(X, Y, Z) :- X>0, kinds_of_coins(Y,A), X1 = X-A, cc(X1, Y, Z1), Y1 = Y-1, cc(X, Y1, Z2), Z = Z1 + Z2.

%count_change(X,Z) :- cc(X,5,Z).

kinds_of_coins(1,1).
kinds_of_coins(2,5).
kinds_of_coins(3,10).
kinds_of_coins(4,25).
kinds_of_coins(5,50).

% the book says that for amount 100 the number is 292

false :- N > 292, cc(100, 5, N).