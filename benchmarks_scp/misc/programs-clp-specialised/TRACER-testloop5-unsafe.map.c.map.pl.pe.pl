new7(A,B,C) :-
   1*A>=10,
   1*B=0,
   1*A+ -1*C=0.
new6(A,B) :-
   1*A=10,
   1*B=10,
   1*C=0,
   new7(A,C,B).
new4(A,B) :-
   -1*A>= -9,
   1*A>=1,
   1*A+ -1*B=0,
   new3(A,B).
new4(A,B) :-
   1*A=10,
   1*B=10,
   new6(A,B).
new3(A,B) :-
   -1*A>= -9,
   1*A>=0,
   1*A+ -1*C= -1,
   1*A+ -1*D= -1,
   new4(D,C).
new2(A) :-
   1*A=0,
   new3(A,B).
new1 :-
   1*A=0,
   new2(A).
false :-
   new1.

