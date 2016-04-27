new10(A,B,C) :-
   1*A+ -2*C>=0,
   -1*A+1*B>=1,
   2*A+ -1*B>=0.
new9(A,B,C) :-
   1*A+ -1*C>=1,
   -1*A+2*C>=0,
   2*A+ -1*B>=2,
   1*B+ -1*D=1,
   new7(A,D,C).
new9(A,B,C) :-
   1*A>=1,
   -1*A+1*C>=0,
   2*A+ -1*B>=2,
   1*B+ -1*D= -2,
   new7(A,D,C).
new7(A,B,C) :-
   1*A>=0,
   2*A+ -1*B>=0,
   -1*A+2*C>=1,
   1*C>=1,
   1*A+ -1*D= -1,
   new9(D,B,C).
new7(A,B,C) :-
   1*A+ -2*C>=0,
   -1*A+1*B>=1,
   2*A+ -1*B>=0,
   new10(A,B,C).
new4(A,B,C,D) :-
   1*A=1,
   1*B=0,
   1*C=0,
   new7(B,C,D).
new3(A,B,C) :-
   1*C>=1,
   1*A=0,
   1*B=0,
   1*D=1,
   new4(D,A,B,C).
new2 :-
   1*C>=1,
   1*A=0,
   1*B=0,
   new3(B,A,C).
new1 :-
   new2.
false :-
   new1.

