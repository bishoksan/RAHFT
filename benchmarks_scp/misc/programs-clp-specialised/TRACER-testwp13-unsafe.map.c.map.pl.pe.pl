new8(A,B,C) :-
   -1*B>=1,
   1*B>= -4,
   1*A=0.
new7(A,B) :-
   -1*A>=1,
   1*A>= -4,
   1*C=0,
   new8(C,A,B).
new5(A,B) :-
   -1*A>=1,
   1*A>= -4,
   new7(A,B).
new4(A,B) :-
   -1*B>=0,
   -1*A>= -9,
   1*A>=6,
   1*A+ -1*C=10,
   new5(C,B).
new3(A,B) :-
   -1*B>=0,
   -1*A>= -9,
   1*A>=6,
   new4(A,B).
new2 :-
   -1*B>= -9,
   -1*A>=0,
   1*B>=6,
   new3(B,A).
new1 :-
   new2.
false :-
   new1.

