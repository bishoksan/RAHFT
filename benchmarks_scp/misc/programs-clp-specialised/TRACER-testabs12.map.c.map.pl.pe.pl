new15(A,B,C,D) :-
   1*A=0,
   1*B=100,
   1*D=0.
new12(A,B,C) :-
   -1*B>=1,
   1*A=100,
   1*D=0,
   new11(A,B,D).
new11(A,B,C) :-
   1*A=100,
   1*C=0,
   1*D=0,
   new15(D,A,B,C).
new10(A,B,C) :-
   -1*B>=1,
   1*A=100,
   new12(A,B,C).
new7(A,B,C) :-
   1*A+ -1*B>=101,
   -1*A>= -99,
   1*A>=0,
   1*A+ -1*D= -1,
   1*B+ -1*E= -1,
   new7(D,E,C).
new7(A,B,C) :-
   -1*B>=1,
   1*A=100,
   new10(A,B,C).
new4(A,B,C,D) :-
   -1*C>=101,
   1*A=1,
   1*E=0,
   new7(E,C,D).

