
fib(X, X):-  X>=1, X=<1.
false:-
    X>5, fib(X,Y), Y<X.
fib(X, Y) :-
        X > 1,
        X2 = X - 2, fib(X2, Y2),
        X1 = X - 1, fib(X1, Y1),
        Y = Y1 + Y2.

